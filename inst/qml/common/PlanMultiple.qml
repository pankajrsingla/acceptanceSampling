//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Group
{
    IntegerField
    { 
        Layout.columnSpan: 2
        name: "lotSizeMult"; label: qsTr("Lot size (N): "); defaultValue: 100; min: 1
    }

    IntegerField
    {
        Layout.columnSpan: 2
        name: "numberOfStages"; id: numberOfStages; label: qsTr("Number of stages"); defaultValue: 2; min: 2; max: 100
    }

    ColumnLayout
    {
        spacing:                                0
        Layout.preferredWidth:					parent.width
        Layout.columnSpan:						2
        
        RowLayout
        {
            Label { text: qsTr("Stage");					Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
            Label { text: qsTr("Sample size (n)");			Layout.preferredWidth: 150 * preferencesModel.uiScale}
            Label { text: qsTr("Acceptance number (c)");	Layout.preferredWidth: 150 * preferencesModel.uiScale}
            Label { text: qsTr("Rejection number (r)");		Layout.preferredWidth: 150 * preferencesModel.uiScale}
        }

        ComponentsList
        {
            name:								"stages"
            addItemManually:                    false
            values:                             numberOfStages.value

            rowComponent: 						RowLayout
            {
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		40 * preferencesModel.uiScale
                    Label
                    {
                        text: 					rowIndex + 1
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		150 * preferencesModel.uiScale

                    IntegerField
                    {
                        id:						sampleSizeMult
                        label: 					""
                        name: 					"sampleSizeMult"
                        defaultValue:           1
                        min:                    1
                        placeholderText:		qsTr("n") + (rowIndex + 1)
                        fieldWidth:				50 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		150 * preferencesModel.uiScale
                    IntegerField
                    {
                        label: 					""
                        name: 					"acceptNumberMult"
                        defaultValue:           1
                        min:                    1
                        placeholderText:		qsTr("c") + (rowIndex + 1)
                        fieldWidth:				50 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		150 * preferencesModel.uiScale
                    IntegerField
                    {
                        label: 					""
                        name: 					"rejectNumberMult"
                        defaultValue:           2
                        min:                    1
                        placeholderText:		qsTr("r") + (rowIndex + 1)
                        fieldWidth:				50 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
            }
        }
    }
}