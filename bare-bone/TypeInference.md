# Type Inference


Type inference is a bare-bone for Haskell. When you know (define) type of some
variables or other expressions, we want to infer the type of new define data or
functions.

## Preliminaries
We first define language setting for Haskell which can help derive mechanism of
type inference. It consists of Expression, Type and Schemes. Expression are
normal Haskell expressions, like Variable, Literatures, and pairs of expressions.
Type are special data that is defined how the type inferred can be recorded and
shown.

```haskell
data Exp     =  EVar EVar            -- x
          |  ELit Lit             -- 0,1,2,true,false
          |  EApp Exp Exp         -- e1 e2
          |  EAbs EVar Exp        -- \x -> e
          |  ELet EVar Exp Exp    -- let x = e1 in e2
          |  EPair Exp Exp        -- (e1, e2)
          deriving (Eq, Ord)

data Type    =  TVar TVar        -- a
          |  TInt             -- Int
          |  TBool            -- Bool
          |  TArr Type Type   -- t1 -> t2
          |  TPair Type Type   -- t1 -> t2
          deriving (Eq, Ord)

data Scheme  =  Forall [TVar] Type       -- forall a. a -> a -> Bool
newtype TypeEnv = TypeEnv (Map.Map EVar Scheme)
```

Here we simply define that there are only two primitive types `Int` and `Bool`.
With the primitives, we can recursively build other types like `TArr` which
denotes a type for function.

Scheme is a special type that I do not exactly catch the point of it, `TODO`.

And there is one more special data defined for the language, which is called
`Environment`. The meaning of it is that all the names and their corresponding
types (`schemes`). Therefore, environment for type inference is actually a `Map`.

## Unification

First, I want to clarify that all the types for Haskell are now defined by above
`Type`. Then think, if we want to do type inference, the most important thing is
substitution. We substitute type-known variables in `Type` with known knowledge.
For example, if we know type variable `a` is `Int`, then when we meet a new type
`a -> a`, we can modify it as `Int -> Int`.

Therefore, we define a class named Substitutable.
```haskell
type Subst = Map.Map TVar Type

class Substitutable a where
  apply     :: Subst -> a -> a
  freeTvars :: a -> Set.Set TVar
```

Notice that `Subst` is much similar with `TypeEnv` above, but they are actually
two different definitions. `Subst` is a map for type variable to a type. `TypeEnv`
is a map for expression variable to a type scheme. You can consider expression
variable is actually variables in expression, like `a` in `a = 2`.

And `apply` does the substitution work. `freeTvars` determines the set of free
type variables. The substitution can only affect free variables in a type
expression. Like in `Forall a. a -> a -> b` you cannot substitute variable `a`.

We define `Type` instance of `Substitutable`. First, do some clarification,
`apply` will modify a `Type` to a `Type` which means it just do some substitution.

```haskell
instance Substitutable Type where
  apply _  TInt            = TInt
  apply _  TBool           = TBool
  apply su t@(TVar a)      = Map.findWithDefault t a su
  apply su (t1 `TArr` t2)  = apply su t1 `TArr` apply su t2

  freeTvars TInt           =  Set.empty
  freeTvars TBool          =  Set.empty
  freeTvars (TVar a)       =  Set.singleton a
  freeTvars (t1 `TArr` t2) =  freeTvars t1 `Set.union` freeTvars t2
```

And use `scheme` to actually define a complete type. And `scheme` instance of
`Substitutable` is shown below. The idea is that for `apply` you can only apply
variables that are not in `as`, and for `freeTvars` the free variables are
variables that are not in `as`.

```haskell
instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as

  freeTvars (Forall as t) = (freeTvars t) `Set.difference` (Set.fromList as)
```

After defining a data of a type class, the direct benefit is that we can use
the functions on the data. After defining instances of `Substitutable`, we can
use `apply` to substitute some `TVar` in target data, and we can get the free
type variables from a target data.

### Unifier: alignment

Work for a unifier is to take in two `Type`s, and then output a `Subst` which
means there are some alignment between `TVar` and `Type`.

So the function signature is that
```haskell
mgu :: Type -> Type -> HM Subst
```
where `HM` is a `State + Error` Monad.

And generally define `mgu`
```haskell
mgu (l `TArr` r) (l' `TArr` r')  = do  s1 <- mgu l l'
                                       s2 <- mgu (apply s1 r) (apply s1 r')
                                       return (s1 `after` s2)
mgu (TVar a) t                   = varAsgn a t
mgu t (TVar a)                   = varAsgn a t
mgu TInt TInt                    = return empSubst
mgu TBool TBool                  = return empSubst
mgu t1 t2                        = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
```

Most simple thing to complete is to align a single `TVar` with a `Type`. The work
is just to assign the `TVar a` to `Type t`. But there are some details in it.
```haskell
varAsgn :: TVar -> Type -> HM Subst
varAsgn a t
  | t == TVar a                  =  return empSubst
  | a `Set.member` (freeTvars t) =  throwError $ "occur check fails: " ++ show a ++ " in " ++ show t
  | otherwise                    =  return $ Map.singleton a t
```
If `a` and `t` are same thing, there is no need for a `Subst`. If `a` is a member
of free variables of `t`, it is a error because there is a recuresive definition.

For `TArr`, first, I get alignment rules from lefts. then apply the left rules to
rights. Finally, return a union of left and right rules.


## Type Inference Algorithm

See we have methods for unify two type variable. But our ultimate goal is to
infer types of actual variables and expressions.

### Generalization and Instantiation

Generalization takes a type `a` and converts it to scheme `Forall a. a`
```haskell
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where
    as = Set.toList $ (freeTvars t) `Set.difference` (freeTvars env)
```

So that we can see generalization is to generalize the free variables in a `Type`
and convert it to a scheme.

Instantiation is to convert a scheme to a type with some fresh variables which
originally are not free variables.

```haskell
instantiate :: Scheme -> HM Type
instantiate (Forall as t) = do
  as' <- mapM (\ _ -> freshTVar "a") as
  let s = Map.fromList $ zip as as'
  return $ apply s t
```

### Main Type inference function

```haskell
> ti :: TypeEnv -> Exp -> HM (Subst, Type)
```
The function expects the precondition that the type environment must contain
bindings for all free variables of the expressions. The output action is a
state-and-error monad m containing a pair of a substitution which records the
type constraints imposed on type variables by the expression, and the inferred
type of the expression.

`TypeEnv`:
1. Insert: when there is new data variable.
2. Update: `apply`. you have new substitution map `Subst`.

`Subst`:
1. Produce new alignment: `mgu` between Types.
