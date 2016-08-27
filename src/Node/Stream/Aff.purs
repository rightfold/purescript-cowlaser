module Node.Stream.Aff
( write
, writeString
, end
) where

import Control.Monad.Aff (Aff, makeAff)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream (Writable)
import Node.Stream as N
import Prelude

write
  :: forall r eff
   . Writable r eff
  -> Buffer
  -> Aff eff Unit
write stream buffer =
  makeAff \_ done -> void (N.write stream buffer (done unit))

writeString
  :: forall r eff
   . Writable r eff
  -> Encoding
  -> String
  -> Aff eff Unit
writeString stream encoding buffer =
  makeAff \_ done -> void (N.writeString stream encoding buffer (done unit))

end :: forall r eff. Writable r eff -> Aff eff Unit
end stream = makeAff \_ done -> N.end stream (done unit)
