#!/usr/bin/env bash

old_file=$(sed -ne 's|.*val file.*"\(.*\)"|\1|p' src/test/scala/test.scala)
sed -i -e "s|\(file.*\)\".*\"|\1\"$1\"|" src/test/scala/test.scala
make test
end=$?
sed -i -e "s|\(file.*\)\".*\"|\1\"$old_file\"|" src/test/scala/test.scala
exit $end
