WriteMyScheme
=============

Implement toy Scheme by "Write Yourself a Scheme in 48 Hours"

# 主な違い

- Write Yourself a Scheme in 48 Hours とはドット対の表現が異なる。

- 幾つかの型は、組み込みの型でないことが明確になるように名前を変更した。

|        | WYS        | WriteMyScheme |
|--------------|------------|---------------|
| 型           |LispVal     | ScmExp        |
| 型           |LispError   | ScmError      |
| データ構築子 | Number     | ScmInt       |
| データ構築子 | String     | ScmString       |
| データ構築子 | Atom       | ScmSymbol       |
| データ構築子 | Bool       | ScmBool       |
