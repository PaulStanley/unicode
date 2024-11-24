#!/bin/bash

echo "Generating InternalEmoji.roc"
roc run package/InternalEmojiGen.roc -- package/

echo "Generating InternalGBP.roc"
roc run package/InternalGBPGen.roc -- package/

echo "Generating GraphemeTest.roc"
roc run package/GraphemeTestGen.roc -- package/

echo "Generating InternalEAW.roc"
roc run package/InternalEAWGen.roc -- package/

echo "Generating InternalComposition.roc"
roc run package/InternalCompositionGen.roc -- package/

echo "Generating InternalDerivedNorm.roc"
roc run package/InternalDerivedNormGen.roc -- package/

echo "Generating InternalNormalizationTest.roc"
roc run package/InternalNormalizationTestGen.roc -- package/
