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
import QtQuick.Controls 2.12
import QtQuick.Layouts
import JASP.Controls
import JASP

Form 
{
    id: form
    property int columns: 2
    LS.LSintrotext {}
    ColorPalette {}

    Section 
    {
        expanded: true
        title: qsTr("Prior Belief")

        CheckBox 
        {
            name:    "priorExpText"
            label:   qsTr("Explanatory text")
            checked: false
        }

        Group 
        {
            title: qsTr("Set your initial belief of the number of species:")
            

            RadioButtonGroup 
            {
                id:    priorType
                name: "priorType"

                RowLayout
                {
                    RadioButton 
                    {
                        value:   "uniform"
                        label:   qsTr("Equal probability for each species count")
                        checked: true
                    }

                    RadioButton 
                    {
                        value: "customized"
                        label:   qsTr("Custom probability for each species count")
                    }

                }
            }

            Group
            {
                visible:   priorType.value === "customized"

                GridLayout 
                {
                    columns: 4
                    rowSpacing: 8
                    columnSpacing: 12

                    Label
                    {
                        text: qsTr("Hypothetical Species count (S)")
                        Layout.preferredWidth: 255
                    }

                    Label
                    {
                        text: qsTr(" 1")
                        Layout.preferredWidth: 60
                    }

                    Label
                    {
                        text: qsTr(" 2")
                        Layout.preferredWidth: 60
                    }

                    Label
                    {
                        text: qsTr(" 3")
                        Layout.preferredWidth: 60
                    }

                    Label
                    {
                        text: qsTr("Prior Probability p(S)")
                        Layout.preferredWidth: 240
                    }

                    DoubleField
                    {
                        name: "priorS1"
                        defaultValue: 1
                        min: 0
                        fieldWidth: 60
                    }

                    DoubleField
                    {
                        name: "priorS2"
                        defaultValue: 1
                        min: 0
                        fieldWidth: 60
                    }

                    DoubleField
                    {
                        name: "priorS3"
                        defaultValue: 1
                        min: 0
                        fieldWidth: 60
                    }
                }
            }

            Group 
            {
                title:    qsTr("Prior table and plot")
                RowLayout
                {
                    DropDown
                    {
                    id: priorDisplay
                    label: qsTr("Displayed hypotheses")
                    name: "priorDisplay"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Species count"),                value: "numS"},
                            { label: qsTr("Species combination"),              value: "comb"},
                        ]
                    }

                    CheckBox
                    {
                        name:    "priorDispPlot"
                        label:   qsTr("Display prior plot")
                        checked: false
                    }
                }   
            }
        }
    }

    Section 
    {
        expanded: true
        title: qsTr("Data")
        columns: 1


        CheckBox
        {
            name:      "dataExpText"
            label:     qsTr("Explanatory text")
            checked:   false
        }


        

        Group
        {
            title: qsTr("Random Sample")

            RowLayout
            {
                spacing: 60
                Button {text: qsTr("Sample an animal") ; onClicked: { sampleTrigger.value = sampleTrigger.value + 1; }}
                Button {text: qsTr("Reset") ; onClicked: { sampleTrigger.value = 0; resetTrigger.value = resetTrigger.value + 1}}
            }

            

            IntegerField 
            {
                id: sampleTrigger

                name: "sampleTrigger"
                defaultValue: 0
                visible: false
            }

            IntegerField 
            {
                id: resetTrigger

                name: "resetTrigger"
                defaultValue: 0
                visible: false
            }
        }
    }

    Section
    {
        expanded:  false
        title:     qsTr("Likelihood")
        columns:   1

        CheckBox
        {
            name:      "likelihoodExpText"
            label:     qsTr("Explanatory text")
            checked:   false
        }

        Group
        {
            title:      qsTr("Likelihood table and plot")
            columns:   2

            CheckBox
            {
                name:      "likelihoodTable"
                label:     qsTr("Table")
                checked:   false

                DropDown
                {
                    id: likelihoodTableDisplay
                    label: qsTr("Displayed hypotheses")
                    name: "likelihoodTableDisplay"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Species count"),                value: "numS"},
                            { label: qsTr("Species combination"),              value: "comb"},
                        ]
                }

                DropDown
                {
                    id: likelihoodTableValue
                    label: qsTr("Displayed value")
                    name: "likelihoodTableValue"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Raw likelihood"),                value: "rawL"},
                            { label: qsTr("Log likelihood"),              value: "logL"},
                        ]
                }

                CheckBox
                {
                    id: likelihoodTableHide
                    label: qsTr("Hide impossible hypotheses")
                    name: "likelihoodTableHide"
                    checked: false
                }
            }

            CheckBox
            {
                name:      "likelihoodPlot"
                label:     qsTr("Plot")
                checked:   false

                DropDown
                {
                    id: likelihoodPlotDisplay
                    label: qsTr("Hypothesis type: ")
                    name: "likelihoodPlotDisplay"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Species count"),                value: "numS"},
                            { label: qsTr("Species combination"),              value: "comb"},
                        ]
                }

                DropDown
                {
                    id: likelihoodPlotData
                    label: qsTr("Likelihood of: ")
                    name: "likelihoodPlotData"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Last sample"),                value: "batch"},
                            { label: qsTr("Overall"),              value: "all"},
                        ]
                }

                

                CheckBox
                {
                    id: likelihoodPlotHide
                    label: qsTr("Hide impossible hypotheses")
                    name: "likelihoodPlotHide"
                    checked: false
                }
            }
            
            
        }

        CheckBox
        {
            id:        showBF
            name:      "showBF"
            label:     qsTr("Bayes factor: compare likelihood")
            checked:   false

            DropDown
            {
                id: bFDisplay
                label: qsTr("Hypothesis type: ")
                name: "bFDisplay"
                visible: showBF.checked
                useExternalBorder: true
                fieldWidth:			 130 * preferencesModel.uiScale
                values:
                    [
                        { label: qsTr("Species count"),                value: "numS"},
                        { label: qsTr("Species combination"),              value: "comb"},
                    ]
            }

            VariablesForm
            {
                visible: bFDisplay.value == "numS" && showBF.checked

                preferredHeight: 150 * preferencesModel.uiScale
                preferredWidth:  550 * preferencesModel.uiScale

                AvailableVariablesList
                {
                    name:   "availNumS"
                    source: [{ values: [
                                { label: "H₁ - 1 species", value: "h1" },
                                { label: "H₂ - 2 species", value: "h2" },
                                { label: "H₃ - 3 species", value: "h3" }
                            ]}]
                    
                }
                AssignedPairsVariablesList
                {
                    name:  "bfPairsSpe"
                    label: qsTr("Selected hypotheses pairs")
                }
            }

            VariablesForm
            {
                visible: bFDisplay.value == "comb" && showBF.checked

                preferredHeight: 150 * preferencesModel.uiScale
                preferredWidth:  550 * preferencesModel.uiScale

                AvailableVariablesList
                {
                    name: "availComb"
                    source: [{ values: [
                        { label: "H₁α - cat", value: "h1a" },
                        { label: "H₁β - horse", value: "h1b" },
                        { label: "H₁γ - pigeon", value: "h1c" },
                        { label: "H₂α - cat, horse", value: "h2a" },
                        { label: "H₂β - cat, pigeon", value: "h2b" },
                        { label: "H₂γ - horse, pigeon", value: "h2c" },
                        { label: "H₃  - cat, horse, pigeon",  value: "h3"  }
                    ]}]

                    preferredHeight: 150 * preferencesModel.uiScale
                }
                AssignedPairsVariablesList
                {
                    name:  "bfPairsComb"
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

        CheckBox
        {
            name:      "posteriorExpText"
            label:     qsTr("Explanatory text")
            checked:   false

        }

        Group
        {
            title:      qsTr("Prior and Posterior")
            columns:   1

            Group
            {
                title:   qsTr("Belief Update")
                columns: 2

                CheckBox
                {
                    name:      "beliefUpdateTable"
                    label:     qsTr("Table")
                    checked:   false

                    DropDown
                    {
                        id: belifeUpdateTableHyp
                        label: qsTr("Hypothesis type: ")
                        name: "bfUpdateTableDisp"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Species count"),                value: "numS"},
                                { label: qsTr("Species combination"),              value: "comb"},
                            ]
                    }

                    DropDown
                    {
                        id: belifeUpdateTableStat
                        label: qsTr("Evidence from: ")
                        name: "bfUpdateTableStat"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Last sample"),                value: "batch"},
                                { label: qsTr("Overall"),              value: "all"},
                            ]
                    }

                    CheckBox
                    {
                        name:   "bfUpdateTableHide"
                        label:  qsTr("Hide impossible hypotheses")
                        checked: false
                    }



                }

                CheckBox
                {
                    name:      "beliefUpdatePlot"
                    label:     qsTr("Plot")
                    checked:   false

                    DropDown
                    {
                        id: belifeUpdatePlotHyp
                        label: qsTr("Hypothesis type: ")
                        name: "bfUpdatePlotDisp"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Species count"),                value: "numS"},
                                { label: qsTr("Species combination"),              value: "comb"},
                            ]
                    }

                    DropDown
                    {
                        id: belifeUpdatePlotStat
                        label: qsTr("Prior from: ")
                        name: "bfUpdatePlotStat"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Last sample"),                value: "batch"},
                                { label: qsTr("Initial"),              value: "all"},
                            ]
                    }

                    CheckBox
                    {
                        name:   "bfUpdatePlotHide"
                        label:  qsTr("Hide impossible hypotheses")
                        checked: false
                    }
                }

            }

            Group
            {
                title:   qsTr("Evidence Accumulation")
                columns: 1

                CheckBox
                {
                    id:       "evidPlot"
                    name:     "evidPlot"
                    label:    qsTr("Plot")
                    checked: false

                    DropDown
                    {
                        id: eviPlotHyp
                        visible: evidPlot.checked
                        label: qsTr("Hypothesis type: ")
                        name: "eviPlotDisp"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Species count"),                value: "numS"},
                                { label: qsTr("Species combination"),              value: "comb"},
                            ]
                    }



                    VariablesForm
                    {
                        visible: eviPlotHyp.value == "numS" && evidPlot.checked
                        preferredHeight: 150 * preferencesModel.uiScale
                        preferredWidth:  550 * preferencesModel.uiScale
                        AvailableVariablesList
                        {
                            name:   "eviAvailNumS"
                            values: [
                                        "H₁ - 1 species",
                                        "H₂ - 2 species",
                                        "H₃ - 3 species"
                                    ]
                            
                        }
                        AssignedVariablesList  
                        { 
                            name: "eviSpe" 
                            label: qsTr("Selected hypotheses")
                        }
                    }

                    VariablesForm
                    {
                        visible: eviPlotHyp.value== "comb" && evidPlot.checked

                        preferredHeight: 150 * preferencesModel.uiScale
                        preferredWidth:  550 * preferencesModel.uiScale

                        AvailableVariablesList
                        {
                            name: "eviAvailComb"
                            values: [
                                "H₁α - cat", 
                                "H₁β - horse",
                                "H₁γ - pigeon", 
                                "H₂α - cat, horse", 
                                "H₂β - cat, pigeon", 
                                "H₂γ - horse, pigeon", 
                                "H₃  - cat, horse, pigeon"
                            ]

                            preferredHeight: 150 * preferencesModel.uiScale
                        }
                        AssignedVariablesList
                        {
                            name:  "eviComb"
                            label: qsTr("Selected hypotheses")
                        }
                    }
                }



            }

            Group
            {
                title: qsTr("Compare Hypotheses")
                columns: 1
                
                RowLayout
                {
                    spacing: 90
                    CheckBox
                    {
                        id: "compPostTable"
                        name:   "compPostTable"
                        label: qsTr("Table")
                        checked: false
                    }

                    CheckBox
                    {
                        id: "compPostPlot"
                        name:   "compPostPlot"
                        label: qsTr("Plot")
                        checked: false
                    }
                }
                
                DropDown
                {
                    id: compPostHyp
                    visible: compPostTable.checked || compPostPlot.checked
                    label: qsTr("Hypothesis type: ")
                    name: "compPostHyp"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Species count"),                value: "numS"},
                            { label: qsTr("Species combination"),              value: "comb"},
                        ]
                
                }
                DropDown
                {
                    id: compPostStat
                    visible: compPostTable.checked || compPostPlot.checked
                    label: qsTr("Evidence from: ")
                    name: "compPostStat"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Last sample"),                value: "batch"},
                            { label: qsTr("Overall"),              value: "all"},
                        ]
                }

                VariablesForm
                {
                    visible: compPostHyp.value == "numS" && (compPostTable.checked || compPostPlot.checked)
                    preferredHeight: 150 * preferencesModel.uiScale
                    preferredWidth:  550 * preferencesModel.uiScale
                    AvailableVariablesList
                    {
                        name:   "compPostAvailNumS"
                        source: [{ values: [
                            { label: qsTr("H₁ - 1 species"), value: "h1" },
                            { label: qsTr("H₂ - 2 species"), value: "h2" },
                            { label: qsTr("H₃ - 3 species"), value: "h3" }
                        ]}]
                    }
                    AssignedPairsVariablesList  
                    { 
                        name: "compPostSpe" 
                        label: qsTr("Selected hypotheses pairs")
                    }
                }

                VariablesForm
                {
                    visible: compPostHyp.value== "comb" && (compPostTable.checked || compPostPlot.checked)
                    preferredHeight: 150 * preferencesModel.uiScale
                    preferredWidth:  550 * preferencesModel.uiScale
                    AvailableVariablesList
                    {
                        name:   "compPostAvailComb"
                        source: [{ values: [
                            { label: qsTr("H₁α - cat"),                  value: "h1a" },
                            { label: qsTr("H₁β - horse"),                value: "h1b" },
                            { label: qsTr("H₁γ - pigeon"),               value: "h1c" },
                            { label: qsTr("H₂α - cat, horse"),           value: "h2a" },
                            { label: qsTr("H₂β - cat, pigeon"),          value: "h2b" },
                            { label: qsTr("H₂γ - horse, pigeon"),        value: "h2c" },
                            { label: qsTr("H₃  - cat, horse, pigeon"),   value: "h3"  }
                        ]}]
                        preferredHeight: 150 * preferencesModel.uiScale
                    }
                    AssignedPairsVariablesList
                    {
                        name:  "compPostComb"
                        label: qsTr("Selected hypotheses pairs")
                    }
                }
            }
        }
    }

    Section 
    {
        expanded: false
        title:    qsTr("Posterior Prediction")
        columns: 1

        CheckBox
        {
            name:      "predExpText"
            label:     qsTr("Explanatory text")
            checked:   false

        }

        Group
        {
            title:      qsTr("Posterior predictive results")
            columns:   2

            CheckBox
            {
                name:      "postPredTable"
                label:     qsTr("Table")
                checked:   false

                DropDown
                {
                    id: postPredTableTyp
                    label: qsTr("Prediction type: ")
                    name: "postPredTableTyp"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Seen vs. Unseen"),                value: "oldNew"},
                            { label: qsTr("By individual species"),              value: "speType"},
                        ]
                }

                DropDown
                {
                    id: postPredTableHyp
                    label: qsTr("Hypothesis type: ")
                    name: "postPredTableHyp"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Species count"),                value: "numS"},
                            { label: qsTr("Species combination"),              value: "comb"},
                        ]
                }

            }
            CheckBox
            {
                name:      "postPredPlot"
                label:     qsTr("Plot")
                checked:   false

                DropDown
                {
                    id: postPredPlotTyp
                    label: qsTr("Prediction type: ")
                    name: "postPredPlotTyp"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Seen vs. Unseen"),                value: "oldNew"},
                            { label: qsTr("By individual species"),              value: "speType"},
                        ]
                }

                DropDown
                {
                    id: postPredPlotStat
                    label: qsTr("Prior type: ")
                    name: "postPredPlotPrior"
                    useExternalBorder: true
                    fieldWidth:			 130 * preferencesModel.uiScale
                    values:
                        [
                            { label: qsTr("Last sample"),              value: "batch"},
                            { label: qsTr("Initial"),                value: "init"},
                        ]
                }
                
                
            } 
        }
        Group
        {
            title: qsTr("Prediction change")
            columns: 1

            CheckBox
            {
                name:      "predChangePlot"
                label:     qsTr("Plot")
                checked:   false

                RowLayout
                {
                    spacing: 40

                    DropDown
                    {
                        id: predChangePlotTyp
                        label: qsTr("Prediction type: ")
                        name: "predChangePlotTyp"
                        useExternalBorder: true
                        fieldWidth:			 130 * preferencesModel.uiScale
                        values:
                            [
                                { label: qsTr("Seen vs. Unseen"),                value: "oldNew"},
                                { label: qsTr("By individual species"),              value: "speType"},
                            ]
                    }
                }
            }
        }
    }
}
