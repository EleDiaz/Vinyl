{-# LANGUAGE TemplateHaskell, TypeOperators, DataKinds, FlexibleInstances #-}

module Data.Vinyl.TH (vinylFields) where

import Language.Haskell.TH

-- | Make basics functions for vinyl's type
vinylFields :: Name -> Q [Dec]
vinylFields name = do
    rei <- reify name
    return $ getType rei

getType :: Info -> [Dec]
getType (TyConI (TySynD _ _ fields)) = concatMap field (extract fields)
getType _ = error "getType: The type don't have this constraint [*] and this cast [ \"some\" ::: Bool, ...]"

field :: (Type, Type) -> [Dec]
-- field (name, ty) = [d| $(return (nameFun name)) = Field :: $(return name) ::: $(return ty) |]
field (n, t) = [ValD (VarP (nameFun n)) (NormalB (SigE (ConE (mkName "Field")) (AppT (AppT (ConT (mkName ":::")) n) t))) []]

nameFun :: Type -> Name
nameFun (LitT (StrTyLit x)) = mkName x
nameFun x = error $ "nameFun: error" ++ (show x)

-- On the left LitT StrTyLit "***", on the right the type
extract :: Type -> [(Type, Type)]
extract (AppT x PromotedNilT) = extract x
extract (AppT PromotedConsT x) = extract x
extract (AppT (AppT _x z) t@(ConT _)) = [(z,t)]
extract (AppT x y) = extract x ++ extract y
extract _x = error "extract: On read AST, this is bug"

{-
(AppT
    (AppT
        PromotedConsT
        (AppT
            (AppT
                (ConT Data.Vinyl.Field.:::)
                (LitT (StrTyLit "name")))
            (ConT GHC.Base.String)))
    (AppT
        (AppT
            PromotedConsT
            (AppT
                (AppT
                    (ConT Data.Vinyl.Field.:::)
                    (LitT (StrTyLit "age")))
                (ConT GHC.Types.Int)))
        (AppT
            (AppT
                PromotedConsT
                (AppT
                    (AppT
                        (ConT Data.Vinyl.Field.:::)
                        (LitT (StrTyLit "sleeping")))
                    (ConT GHC.Types.Bool)))
             PromotedNilT)))

-}
