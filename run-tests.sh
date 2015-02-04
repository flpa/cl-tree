sbcl --noinform --eval "(require 'asdf)" \
                --eval "(asdf:operate 'asdf:test-op 'cl-tree/test)" \
                --quit
