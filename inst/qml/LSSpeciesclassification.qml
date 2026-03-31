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
import JASP
import JASP.Controls
import QtQuick
import QtQuick.Layouts

Form {
    columns: 1

    RadioButtonGroup {
        id: inputType

        name: "inputType"
        columns: 3
        title: qsTr("Input type")

        RadioButton {
            name: "randsamp"
            label: qsTr("Random Sampling")
            checked: true
        }

        RadioButton {
            name: "speandnum"
            label: qsTr("Enter Species and Number")
        }

        RadioButton {
            name: "sampseq"
            label: qsTr("Enter Sequence")
        }

    }

    Section {
        expanded: true
        title: qsTr("Data")

        Group {
            visible: inputType.value === "randsamp"
            title: qsTr("Random Sample")

            FormulaField {
                name: "nsample"
                label: qsTr("Number of samples")
                min: 0
                defaultValue: 1
                fieldWidth: 45
            }

            IntegerField {
                id: redrawTrigger

                name: "redrawTrigger"
                defaultValue: 0
                visible: false
            }

            Button {
                text: qsTr("Draw new sample")
                onClicked: {
                    redrawTrigger.value = redrawTrigger.value + 1;
                }
            }

        }

    }

}
