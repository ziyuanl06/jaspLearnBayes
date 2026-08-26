// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

import "../qml/qml_components" as LS
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Form 
{
    id: form
    columns: 2

    LS.LSintrotext {}
    ColorPalette {}


    Section
    {
        expanded: true
        title: qsTr("Prior Belief")

        Group
        {
            title: qsTr("Model")
            columns: 1

            RowLayout
            {
                spacing: 16 * preferencesModel.uiScale

                DropDown
                {
                    id:               modelTypeDD
                    name:             "modelType"
                    label:            qsTr("Distribution")
                    useExternalBorder: true
                    fieldWidth:       130 * preferencesModel.uiScale
                    values:
                    [
                        { label: qsTr("Point"),             value: "point"   },
                        { label: qsTr("Uniform"),           value: "uniform" },
                        { label: qsTr("Poisson"),           value: "poisson" },
                        { label: qsTr("Negative binomial"), value: "negbino" }
                    ]
                }
            }

            RowLayout
            {
                spacing: 8 * preferencesModel.uiScale

                IntegerField
                {
                    label:      qsTr("N")
                    name:       "modelPointPriorN"
                    visible:    modelTypeDD.currentValue === "point"
                    value:      1
                    min:        1
                    max:        10
                    fieldWidth: 60 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("How many different species do you think are there on the island?")
                }

                IntegerField
                {
                    label:      qsTr("λ")
                    name:       "modelPoissonLambda"
                    visible:    modelTypeDD.currentValue === "poisson"
                    value:      1
                    min:        1
                    fieldWidth: 60 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("What is the most likely number of species on the island?")
                }

                IntegerField
                {
                    label:      qsTr("μ")
                    name:       "modelNbMu"
                    visible:    modelTypeDD.currentValue === "negbino"
                    value:      1
                    min:        1
                    fieldWidth: 60 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("What is the most likely number of species on the island?")
                }

                IntegerField
                {
                    label:      qsTr("ϕ")
                    name:       "modelNbPhi"
                    visible:    modelTypeDD.currentValue === "negbino"
                    value:      1
                    min:        1
                    fieldWidth: 60 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("How certain are you? Larger values mean more certainty.")
                }

                IntegerField
                {
                    id:         modelMinField
                    label:      qsTr("Min")
                    name:       "modelMin"
                    visible:    modelTypeDD.currentValue !== "point"
                    value:      1
                    min:        1
                    max:        10
                    fieldWidth: 50 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("What do you think is the minimum possible number of different species on the island?")
                }

                IntegerField
                {
                    label:      qsTr("Max")
                    name:       "modelMax"
                    visible:    modelTypeDD.currentValue !== "point"
                    value:      10
                    min:        modelMinField.value
                    max:        10
                    fieldWidth: 50 * preferencesModel.uiScale
                    showBorder: true
                    toolTip:    qsTr("What do you think is the maximum possible number of different species on the island?")
                }
            }

            RowLayout
            {
                spacing: 20 * preferencesModel.uiScale

                CheckBox
                {
                    name:    "modelShowExplain"
                    label:   qsTr("Show explanation")
                    toolTip: qsTr("Display a text explanation of this model in the results.")
                }

                CheckBox
                {
                    name:    "modelShowPlot"
                    label:   qsTr("Show distribution plot")
                    toolTip: qsTr("Display a distribution plot of this model in the results.")
                }
            }
        }

        Group
        {
            title: qsTr("Prior summary statistics")
            name: "priorTableOptions"

            RowLayout 
            {
                spacing: 30

                CheckBox 
                {
                    name: "priorMean"
                    label: qsTr("Mean")
                    checked: false
                }

                CheckBox 
                {
                    name: "priorMedian"
                    label: qsTr("Median")
                    checked: false
                }
                CheckBox 
                {
                    name: "priorMode"
                    label: qsTr("Mode")
                    checked: false
                }

                CheckBox 
                {
                    name: "priorSD"
                    label: qsTr("Standard deviation")
                    checked: false
                }
            }

            Row 
            {
                spacing:               4 * preferencesModel.uiScale

                CheckBox 
                {
                    id:  minandmax
                    name: "variableBetween"
                    label: "Probability of s between (min and max included):"
                    checked: false
                }

                IntegerField 
                {
                    id:   mincut
                    name: "priorUserMin"
                    label: "Min"
                    fieldWidth: 40 * preferencesModel.uiScale
                    value: 1
                    min: 1
                    enabled: minandmax.checked 
                    showBorder: true
                }

                IntegerField 
                {
                    name: "priorUserMax"
                    label: "Max"
                    fieldWidth: 40 * preferencesModel.uiScale
                    value: 5
                    min: mincut.value
                    enabled: minandmax.checked
                    showBorder: true
                }
            }
        }
    }

    Section
    {
        expanded: true
        title: qsTr("Data")

        Group
        {
            visible: inputType.value === "randsamp"
            title: qsTr("Random sample")


            RowLayout
            {
                spacing: 50
                CheckBox
                {
                    id: selectIslandBox
                    name: "selectisland"
                    label: qsTr("Select an island")
                    checked: false
                }

                IntegerField
                {
                    id: islandidField
                    name: "islandid"
                    label: qsTr("Enter island id")
                    enabled: selectIslandBox.checked
                    defaultValue: 1
                    min: 1
                }

            }

            RowLayout
            {
                spacing: 60

                IntegerField
                {
                    id: rdsampleinput
                    name: "nsample"
                    label: qsTr("Sample size")
                    min: 1
                    max: 100
                    defaultValue: 1
                    fieldWidth: 45
                }

                Button {text: qsTr("Generate sample") ; onClicked: { redrawTrigger.value = redrawTrigger.value + 1; }}
                Button {text: qsTr("Reset") ; onClicked: { redrawTrigger.value = 0; rdsampleinput.value = 1; resetSample.value = resetSample.value + 1}}

            }

            IntegerField
            {
                id: redrawTrigger

                name: "redrawTrigger"
                defaultValue: 0
                visible: false
            }

            IntegerField
            {
                id: resetSample
                name: "resetSample"
                defaultValue: 0
                visible: false
            }


        }
        Group
        {
            title: qsTr("Plot")

            RowLayout
            {
                spacing: 80

                CheckBox
                {
                    name: "barBatch"
                    label: qsTr("The current sample")
                    checked: false
                }

                CheckBox
                {
                    name: "barAllSample"
                    label: qsTr("Overall")
                    checked: false
                }
            }
        }
    }

    Section
    {
        expanded: false
        title: qsTr("Abundance")
        columns: 1

        Group{
            title: qsTr("Species abundance")
            name: "speAbundOptions"

            DropDown{
                id: abundanceStat
                label: qsTr("Estimate")
                name: "abundanceStat"
                useExternalBorder: true
                fieldWidth:			 95 * preferencesModel.uiScale
                values:
                [
                    { label: qsTr("mean"),                value: "mean"},
                    { label: qsTr("median"),              value: "median"},
                    { label: qsTr("mode"),              value: "mode"},
                ]
            }


            CheckBox {
                name: "despAbundanceTable"
                label: qsTr("Table")
                checked: false
            }

            CheckBox {
                name: "despAbundancePlot"
                label: qsTr("Plots")
                checked: false

                CheckBox {
                    name: "despAbundancePlotExp"
                    label: qsTr("Explain plots")
                    checked: false
                }

                CheckBox {
                    name: "despAbundancePlotEst"
                    label: qsTr("Display estimates")
                    checked: false
                }

                CheckBox {
                    name: "despAbundancePlotSameScale"
                    label: qsTr("Enforce same scale")
                    checked: false
                }
            }
        }
    }
    
    Section 
    {
        expanded: false
        title: qsTr("Likelihood")
        columns: 1

        CheckBox
        {
            name: "bioLikeTable"
            label: qsTr("Table")
            checked: false

            DropDown
            {
                id: bioLikeTableDisp
                label: qsTr("Displayed value: ")
                name: "bioLikeTableDisp"
                useExternalBorder: true
                fieldWidth:			 130 * preferencesModel.uiScale
                values:
                [
                    { label: qsTr("Raw likelihood"), value: "raw"},
                    { label: qsTr("Log likelihood"), value: "log"}
                ]
            }

            CheckBox
            {
                name: "bioLikeTableHide"
                label: qsTr("Hide impossible hypotheses")
                checked: false
            }
        }

        CheckBox
        {
            id: showBioBF
            name: "showBioBF"
            label: qsTr("Bayes factor: compare likelihood")
            checked: false

            DropDown
            {
                id: bioLikeBFData
                label: qsTr("Likelihood of: ")
                name: "bioLikeBFData"
                useExternalBorder: true
                fieldWidth:			 130 * preferencesModel.uiScale
                values:
                [
                    { label: qsTr("The current batch"), value: "batch"},
                    { label: qsTr("Overall"), value: "all"}
                ]
            }

            VariablesForm
            {
                visible: showBioBF.checked

                preferredHeight: 200 * preferencesModel.uiScale

                AvailableVariablesList
                {
                    name:   "bioAvailNumS"
                    source: [{ values: [
                                { label: "H₁ - 1 species", value: "h1" },
                                { label: "H₂ - 2 species", value: "h2" },
                                { label: "H₃ - 3 species", value: "h3" },
                                { label: "H₄ - 4 species", value: "h4" },
                                { label: "H₅ - 5 species", value: "h5" },
                                { label: "H₆ - 6 species", value: "h6" },
                                { label: "H₇ - 7 species", value: "h7" },
                                { label: "H₈ - 8 species", value: "h8" },
                                { label: "H₉ - 9 species", value: "h9" },
                                { label: "H₁₀ - 10 species", value: "h10" }
                            ]}]
                    
                }
                AssignedPairsVariablesList
                {
                    name:  "bioBFPairsSpe"
                    label: qsTr("Selected hypotheses pairs")
                }
            }
        }
    }

    Section
    {
        expanded: false
        title: qsTr("Posterior Belief")
        columns: 1

        Group
        {
            title:   qsTr("Species count")
            columns: 1

            Group
            {
                title:   qsTr("Belief update")
                columns: 2

                CheckBox
                {
                    name:    "beliefUpdateTable"
                    label:   qsTr("Table")
                    checked: false

                    DropDown
                    {
                        id:                beliefUpdateTableStat
                        label:             qsTr("Evidence from: ")
                        name:              "beliefUpdateTableStat"
                        useExternalBorder: true
                        fieldWidth:        130 * preferencesModel.uiScale
                        values:
                        [
                            { label: qsTr("Last sample"), value: "batch" },
                            { label: qsTr("Overall"),     value: "all"   }
                        ]
                    }

                    CheckBox
                    {
                        name:    "beliefUpdateTableHide"
                        label:   qsTr("Hide impossible hypotheses")
                        checked: false
                    }
                }

                CheckBox
                {
                    name:    "beliefUpdatePlot"
                    label:   qsTr("Plot")
                    checked: false

                    DropDown
                    {
                        id:                beliefUpdatePlotStat
                        label:             qsTr("Prior from: ")
                        name:              "beliefUpdatePlotStat"
                        useExternalBorder: true
                        fieldWidth:        130 * preferencesModel.uiScale
                        values:
                        [
                            { label: qsTr("Last sample"), value: "batch"   },
                            { label: qsTr("Initial"),     value: "initial" }
                        ]
                    }

                    CheckBox
                    {
                        name:    "beliefUpdatePlotHide"
                        label:   qsTr("Hide impossible hypotheses")
                        checked: false
                    }
                }
            }

            Group
            {
                title:   qsTr("Evidence accumulation")
                columns: 1

                CheckBox
                {
                    id:      evidAccPlot
                    name:    "evidAccPlot"
                    label:   qsTr("Plot")
                    checked: false

                    VariablesForm
                    {
                        visible:         evidAccPlot.checked
                        preferredHeight: 200 * preferencesModel.uiScale

                        AvailableVariablesList
                        {
                            name:   "evidAccAvailNumS"
                            source: [{ values: [
                                { label: "H₁ - 1 species",   value: "h1"  },
                                { label: "H₂ - 2 species",   value: "h2"  },
                                { label: "H₃ - 3 species",   value: "h3"  },
                                { label: "H₄ - 4 species",   value: "h4"  },
                                { label: "H₅ - 5 species",   value: "h5"  },
                                { label: "H₆ - 6 species",   value: "h6"  },
                                { label: "H₇ - 7 species",   value: "h7"  },
                                { label: "H₈ - 8 species",   value: "h8"  },
                                { label: "H₉ - 9 species",   value: "h9"  },
                                { label: "H₁₀ - 10 species", value: "h10" }
                            ]}]
                        }

                        AssignedVariablesList
                        {
                            name:  "evidAccSelected"
                            label: qsTr("Selected hypotheses")
                        }
                    }
                }
            }

            Group
            {
                title:   qsTr("Compare hypotheses")
                columns: 2

                CheckBox
                {
                    id:      compPostTable
                    name:    "compPostTable"
                    label:   qsTr("Table")
                    checked: false
                }

                DropDown
                {
                    visible:           compPostTable.checked
                    label:             qsTr("Evidence from: ")
                    name:              "compPostStat"
                    useExternalBorder: true
                    fieldWidth:        130 * preferencesModel.uiScale
                    values:
                    [
                        { label: qsTr("Last sample"), value: "batch" },
                        { label: qsTr("Overall"),     value: "all"   }
                    ]
                }

                VariablesForm
                {
                    visible:         compPostTable.checked
                    preferredHeight: 200 * preferencesModel.uiScale

                    AvailableVariablesList
                    {
                        name:   "compPostAvailNumS"
                        source: [{ values: [
                            { label: "H₁ - 1 species",   value: "h1"  },
                            { label: "H₂ - 2 species",   value: "h2"  },
                            { label: "H₃ - 3 species",   value: "h3"  },
                            { label: "H₄ - 4 species",   value: "h4"  },
                            { label: "H₅ - 5 species",   value: "h5"  },
                            { label: "H₆ - 6 species",   value: "h6"  },
                            { label: "H₇ - 7 species",   value: "h7"  },
                            { label: "H₈ - 8 species",   value: "h8"  },
                            { label: "H₉ - 9 species",   value: "h9"  },
                            { label: "H₁₀ - 10 species", value: "h10" }
                        ]}]
                    }

                    AssignedPairsVariablesList
                    {
                        name:  "compPostPairs"
                        label: qsTr("Selected hypotheses pairs")
                    }
                }
            }
        }

        Group
        {
            title:   qsTr("Species abundance")
            columns: 1

            DropDown
            {
                id:                margAbundanceStat
                label:             qsTr("Estimate: ")
                name:              "margAbundanceStat"
                useExternalBorder: true
                fieldWidth:        100 * preferencesModel.uiScale
                values:
                [
                    { label: qsTr("Mean"),   value: "mean"   },
                    { label: qsTr("Median"), value: "median" },
                    { label: qsTr("Mode"),   value: "mode"   }
                ]
            }

            RowLayout
            {
                spacing: 200 * preferencesModel.uiScale
                CheckBox
                {
                    name:    "margAbundanceTable"
                    label:   qsTr("Table")
                    checked: false
                }

                CheckBox
                {
                    name:    "margAbundancePlot"
                    label:   qsTr("Plot")
                    checked: false
                }
            }

            
        }
    }



    Section
    {
        
        expanded: false
        title: qsTr("Posterior Prediction")
        columns: 1

        Group
        {
            columns: 2

            CheckBox
            {
                name:    "predTable"
                label:   qsTr("Table")
                checked: false

                DropDown
                {
                    id:                 predTableTyp
                    label:              qsTr("Prediction type: ")
                    name:               "predTableTyp"
                    useExternalBorder:  true
                    fieldWidth:         130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Seen vs. unseen"),            value: "oldNew"},
                            { label: qsTr("By individual species"),      value: "speType"},
                        ]
                }
            }

            CheckBox
            {
                name:    "predPlot"
                label:   qsTr("Plot")
                checked: false

                DropDown
                {
                    id:                 predPlotTyp
                    label:              qsTr("Prediction type: ")
                    name:               "predPlotTyp"
                    useExternalBorder:  true
                    fieldWidth:         130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Seen vs. unseen"),            value: "oldNew"},
                            { label: qsTr("By individual species"),      value: "speType"},
                        ]
                }
            }
        }

        
    }

}
