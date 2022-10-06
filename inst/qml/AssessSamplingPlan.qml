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
			// something
		}

		Common.RiskPoints
		{
			// something
		}

		Common.Distribution
		{
			// something
		}

		// Group
		// {
		// 	IntegerField { name: "sampleSize"; label: qsTr("Sample size (n): "); defaultValue: 10; min: 1 }
		// 	IntegerField { name: "acceptNumber"; label: qsTr("Acceptance number (c): "); defaultValue: 4 }
		// 	IntegerField { name: "rejectNumber"; label: qsTr("Rejection number (r): "); defaultValue: 5 }
		// }

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	title: qsTr("PRP (Producer Risk Point)")
		// 	Group
		// 	{
		// 		columns: 2
		// 		Text { text: qsTr("Quality Level: [P(defect)]") }
		// 		DoubleField{ name: "pd_prp"; label: qsTr(""); negativeValues: false; defaultValue: 0.1; min: 0; max: 1 }
		// 		Text { text: qsTr("Acceptance Probability: [Least P(accept)]") }
		// 		DoubleField{ name: "pa_prp"; label: qsTr(""); negativeValues: false; defaultValue: 1; min: 0; max: 1 }
		// 	}
		// }
		
		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	title: qsTr("CRP (Consumer Risk Point)")
		// 	Group
		// 	{
		// 		columns: 2
		// 		Text { text: qsTr("Quality Level: [P(defect)]") }
		// 		DoubleField { name: "pd_crp"; label: qsTr(""); negativeValues: false; defaultValue: 0.2; min: 0; max: 1 }
		// 		Text { text: qsTr("Acceptance Probability: [Maximum P(accept)]") }
		// 		DoubleField { name: "pa_crp"; label: qsTr(""); negativeValues: false; defaultValue: 1; min: 0; max: 1 }
		// 	}
		// }

		// RadioButtonGroup
		// {
		// 	title: qsTr("Distribution")
		// 	name: "distribution"
		// 	RadioButton { value: "binom"; label: qsTr("Binomial"); checked: true }
		// 	Group
		// 	{
		// 		RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom }
		// 		IntegerField { name: "lotSize"; label: qsTr("Lot size (N): "); defaultValue: 100; min: 1; enabled: hypergeom.checked }
		// 	}
		// 	RadioButton { value: "poisson"; label: qsTr("Poisson") }
		// }

		Group
		{
			Layout.rowSpan: 2
			CheckBox { label: qsTr("Show OC Curve"); name: "showOCCurveSingle" }
			CheckBox { label: qsTr("Show summary"); name: "showSummarySingle" }
		}
	}

	Section
	{
		title: qsTr("Assess Multiple Sampling Plan")

		Common.PlanMultiple
		{
			// something
		}

		Common.RiskPoints
		{
			// something
		}

		Common.Distribution
		{
			// something
		}

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	TextField { name: "sampleSize_mult"; label: qsTr("Sample sizes (n1,n2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// 	TextField { name: "acceptNumber_mult"; label: qsTr("Acceptance numbers (c1,c2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// 	TextField { name: "rejectNumber_mult"; label: qsTr("Rejection numbers (r1,r2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// }

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	title: qsTr("PRP (Producer Risk Point)")
		// 	Group
		// 	{
		// 		columns: 2
		// 		Text { text: qsTr("Quality Level: [P(defect)]") }
		// 		DoubleField{ name: "pd_prp_mult"; label: qsTr(""); negativeValues: false; defaultValue: 0.1; min: 0; max: 1 }
		// 		Text { text: qsTr("Acceptance Probability: [Least P(accept)]") }
		// 		DoubleField{ name: "pa_prp_mult"; label: qsTr(""); negativeValues: false; defaultValue: 1; min: 0; max: 1 }
		// 	}
		// }
	
		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	title: qsTr("CRP (Consumer Risk Point)")
		// 	Group
		// 	{
		// 		columns: 2
		// 		Text { text: qsTr("Quality Level: [P(defect)]") }
		// 		DoubleField { name: "pd_crp_mult"; label: qsTr(""); negativeValues: false; defaultValue: 0.2; min: 0; max: 1 }
		// 		Text { text: qsTr("Acceptance Probability: [Maximum P(accept)]") }
		// 		DoubleField { name: "pa_crp_mult"; label: qsTr(""); negativeValues: false; defaultValue: 1; min: 0; max: 1 }
		// 	}
		// }

		// RadioButtonGroup
		// {
		// 	title: qsTr("Distribution")
		// 	name: "distribution_mult"
		// 	RadioButton { value: "binom"; label: qsTr("Binomial"); checked: false }
		// 	Group
		// 	{
		// 		RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom_mult }
		// 		IntegerField { name: "lotSize_mult"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; enabled: hypergeom_mult.checked }
		// 	}
		// 	RadioButton { value: "poisson"; label: qsTr("Poisson") }
		// }

		Group
		{
			Layout.rowSpan: 2
			CheckBox { label: qsTr("Show OC Curve"); name: "showOCCurveMult" }
			CheckBox { label: qsTr("Show summary"); name: "showSummaryMult" }
		}
	}
}
