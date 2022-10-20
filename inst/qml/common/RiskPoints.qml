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
    property string suffix: ""
    Group
    {
        columns: 2
        // Layout.columnSpan: 2
        Text { text: qsTr("Acceptable Quality Level (AQL)") }
        DoubleField{ name: "pd_prp" + suffix; label: qsTr(""); negativeValues: false; defaultValue: 0.05; min: 0; max: 1 }
        Text { text: qsTr("Rejectable Quality Level (RQL / LTPD)") }
        DoubleField { name: "pd_crp" + suffix; label: qsTr(""); negativeValues: false; defaultValue: 0.15; min: 0; max: 1 }
    }
    
    Group
    {
        columns: 2
        // Layout.columnSpan: 2
        Text { text: qsTr("Producer's Risk (α)") }
        DoubleField{ name: "pa_prp" + suffix; label: qsTr(""); negativeValues: false; defaultValue: 0.05; min: 0; max: 1 }
        Text { text: qsTr("Consumer's Risk (β)") }
        DoubleField { name: "pa_crp" + suffix; label: qsTr(""); negativeValues: false; defaultValue: 0.10; min: 0; max: 1 }
    }
}