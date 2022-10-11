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
		title: qsTr("Assess Single Sampling Plan")

		Common.PlanSingle
		{
			suffix: "Single"
			Layout.columnSpan: 2			
		}

		Common.RiskPoints
		{
			suffix: "Single"
			Layout.columnSpan: 2
		}

		Common.Distribution
		{
			suffix: "Single"
			Layout.columnSpan: 2
		}

		Group
		{
			Layout.columnSpan: 2
			CheckBox { label: qsTr("Show OC Curve"); name: "showOCCurveSingle" }
			CheckBox { label: qsTr("Show summary"); name: "showSummarySingle" }
		}
	}

	Section
	{
		title: qsTr("Assess Multiple Sampling Plan")

		Common.PlanMultiple
		{
			Layout.columnSpan: 2
		}

		Common.RiskPoints
		{
			suffix: "Mult"
			Layout.columnSpan: 2
		}

		Common.Distribution
		{
			suffix: "Mult"
			Layout.columnSpan: 2
		}

		Group
		{
			CheckBox { label: qsTr("Show OC Curve"); name: "showOCCurveMult" }
			CheckBox { label: qsTr("Show summary"); name: "showSummaryMult" }
		}
	}
}
