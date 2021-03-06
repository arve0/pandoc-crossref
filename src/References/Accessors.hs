module References.Accessors where

import References.Types
import Util.Accessor

imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})
