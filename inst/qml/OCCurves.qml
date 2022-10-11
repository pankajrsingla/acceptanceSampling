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
import "./common" as Common

Form
{
	Section
	{
		title: qsTr("First OC Curve")

		Common.PlanSingle
		{
			suffix: "1"
			Layout.columnSpan: 2
		}
		
		Common.Distribution
		{
			suffix: "1"
			Layout.columnSpan: 2
		}

		CheckBox{ name: "showSummary1"; label: qsTr("Show summary (first plan)") }
	}

	Section
	{
		title: qsTr("Second OC Curve")
		enabled: acceptNumber1.value > 0 && rejectNumber1.value > 0

		Common.PlanSingle
		{
			suffix: "2"
			Layout.columnSpan: 2
		}
		
		Common.Distribution
		{
			suffix: "2"
			Layout.columnSpan: 2
		}

		CheckBox{ name: "showSummary2"; label: qsTr("Show summary (second plan)") }
	}

	Section
	{
		title: qsTr("Third OC Curve")
		enabled: acceptNumber2.value > 0 && rejectNumber2.value > 0

		Common.PlanSingle
		{
			suffix: "3"
			Layout.columnSpan: 2
		}
		
		Common.Distribution
		{
			suffix: "3"
			Layout.columnSpan: 2
		}

		CheckBox{ name: "showSummary3"; label: qsTr("Show summary (third plan)") }
	}

	Section
	{
		title: qsTr("Multiple Sampling Plan")

		Common.PlanMultiple
		{
			Layout.columnSpan: 2
		}

		Common.Distribution
		{
			suffix: "Mult"
			Layout.columnSpan: 2
		}

		CheckBox{ name: "showSummaryMult"; label: qsTr("Show summary (multiple sampling plan)") }
	}
}