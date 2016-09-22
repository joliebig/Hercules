#!/bin/bash
#if [[ ! $1 == *.txt ]]; then
#    echo "No .txt file provided"
#    exit
#fi

#csvFile=$(echo $1 | sed 's/.txt/.csv/g')

resultDirectory=/home/$USER/perf_results_chimaira
herculesDir=$(pwd)
resultFile=$resultDirectory/results.csv

#rm -rf $resultDirectory/*.csv
header="RunID,InputMode,PredictMode,ConfigID,PredictedTime,Variance,PerfTime,SimTime,VarTime,AccPerf,AccVar"

#echo $header #>> $resultFile

#featurewise predicts allyes
echo "featurewise predicts allyes"
rm -rf $resultDirectory/featurewisePredictsAllyes.csv
echo $header >> $resultDirectory/featurewisePredictsAllyes.csv
for i in `seq 0 299`; do
    simTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_ay.txt | grep -o '[0-9.]*')
    varTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_ay.txt | grep -o '[0-9.]*')
    perfTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_ay.txt | grep -o '[0-9.]*')

    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im featurewise -pm allyes -f $resultDirectory/$i/)
    predictedTime=$(echo $prediction | grep -o -E '^Predicted: [0-9.]+ ms' | grep -o '[0-9.]*')
    predictedVariance=$(echo $prediction | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
    csvLine="$i,ft,ay,0,$predictedTime,$predictedVariance,$perfTimeAy,$simTimeAy,$varTimeAy"
    echo $csvLine >> $resultDirectory/featurewisePredictsAllyes.csv
done

#pairwise predicts allyes
echo "pairwise predicts allyes"
rm -rf $resultDirectory/pairwisePredictsAllyes.csv
echo $header >> $resultDirectory/pairwisePredictsAllyes.csv
for i in `seq 0 299`; do
    simTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_ay.txt | grep -o '[0-9.]*')
    varTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_ay.txt | grep -o '[0-9.]*')
    perfTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_ay.txt | grep -o '[0-9.]*')

    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im pairwise -pm allyes -f $resultDirectory/$i/)
    predictedTime=$(echo $prediction | grep -o -E '^Predicted: [0-9.]+ ms' | grep -o '[0-9.]*')
    predictedVariance=$(echo $prediction | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
    csvLine="$i,pr,ay,0,$predictedTime,$predictedVariance,$perfTimeAy,$simTimeAy,$varTimeAy"
    echo $csvLine >> $resultDirectory/pairwisePredictsAllyes.csv
done

#random predicts allyes
echo "random predicts allyes"
rm -rf $resultDirectory/randomPredictsAllyes.csv
echo $header >> $resultDirectory/randomPredictsAllyes.csv
for i in `seq 0 299`; do
    simTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_ay.txt | grep -o '[0-9.]*')
    varTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_ay.txt | grep -o '[0-9.]*')
    perfTimeAy=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_ay.txt | grep -o '[0-9.]*')

    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im random -pm allyes -f $resultDirectory/$i/)
    predictedTime=$(echo $prediction | grep -o -E '^Predicted: [0-9.]+ ms' | grep -o '[0-9.]*')
    predictedVariance=$(echo $prediction | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
    csvLine="$i,rnd,ay,0,$predictedTime,$predictedVariance,$perfTimeAy,$simTimeAy,$varTimeAy"
    echo $csvLine >> $resultDirectory/randomPredictsAllyes.csv
done

exit
# featurewise predicts pairwise
echo "featurewise predicts pairwise"
rm -rf $resultDirectory/featurewisePredictsPairwise.csv
echo $header >> $resultDirectory/featurewisePredictsPairwise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im featurewise -pm pairwise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_pr_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_pr_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_pr_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,ft,pr,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/featurewisePredictsPairwise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

# pairwise predicts featurewise
echo "pairwise predicts featurewise"
rm -rf $resultDirectory/pairwisePredictsFeaturewise.csv
echo $header >> $resultDirectory/pairwisePredictsFeaturewise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im pairwise -pm featurewise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_ft_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_ft_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_ft_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,pr,ft,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/pairwisePredictsFeaturewise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

#random predicts featurewise
echo "random predicts featurewise"
rm -rf $resultDirectory/randomPredictsFeaturewise.csv
echo $header >> $resultDirectory/randomPredictsFeaturewise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im random -pm featurewise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_rnd_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,rnd,ft,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/randomPredictsFeaturewise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

#random predicts pairwise
echo "random predicts pairwise"
rm -rf $resultDirectory/randomPredictsPairwise.csv
echo $header >> $resultDirectory/randomPredictsPairwise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im random -pm pairwise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_rnd_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,rnd,pr,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/randomPredictsPairwise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

#allyes predicts featurewise
echo "allyes predicts featurewise"
rm -rf $resultDirectory/allyesPredictsFeaturewise.csv
echo $header >> $resultDirectory/allyesPredictsFeaturewise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im allyes -pm featurewise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_ft_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_ft_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_ft_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,ay,ft,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/allyesPredictsFeaturewise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

#allyes predicts pairwise
echo "allyes predicts pairwise"
rm -rf $resultDirectory/allyesPredictsPairwise.csv
echo $header >> $resultDirectory/allyesPredictsPairwise.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im allyes -pm pairwise -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_pr_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_pr_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_pr_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,ay,pr,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/allyesPredictsPairwise.csv


    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

#allyes predicts random
echo "allyes predicts random"
rm -rf $resultDirectory/allyesPredictsRandom.csv
echo $header >> $resultDirectory/allyesPredictsRandom.csv
for i in `seq 0 299`; do
    prediction=$(java -jar $herculesDir/PerfTimes.jar -cs sqlite -im allyes -pm random -f $resultDirectory/$i/)
    counter=0
    while read -r line
    do
        paddedCounter=$(printf %03d $counter)
        simTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/sim_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        varTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/var_rnd_$paddedCounter.txt | grep -o '[0-9.]*')
        perfTime=$(grep -o -E 'Total time: [0-9.]+ ms' $resultDirectory/$i/perf_rnd_$paddedCounter.txt | grep -o '[0-9.]*')

        predictedTime=$(echo $line | grep -o -E 'Predicted: [0-9.]+' | grep -o '[0-9.]*')
        predictedVariance=$(echo $line | grep -o -E '± [0-9.]+ ms' | grep -o '[0-9.]*')
        csvLine="$i,ay,rnd,$counter,$predictedTime,$predictedVariance,$perfTime,$simTime,$varTime"
        counter=$(( $counter + 1))
        echo $csvLine >> $resultDirectory/allyesPredictsRandom.csv
    done < <(echo $prediction | grep -o -E 'Predicted: [0-9.]+ ms ± [0-9.]+ ms')
done

exit