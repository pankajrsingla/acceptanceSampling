import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspAcceptanceSampling"
	title		: qsTr("Acceptance Sampling")
	description	: qsTr("Sampling for acceptance")
	icon		: "choice.png"
	version		: "0.16.4"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	requiresData: false

	GroupTitle
	{
		title:	qsTr("Attribute Sampling")
		// icon:	"analysis-classical-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Analyze Sampling Plan")
		qml:	"AnalyzeAttributePlan.qml"
		func:	"AnalyzeAttributePlan"
	}

	Analysis
	{
		title:	qsTr("Create Sampling Plan")
		qml:	"CreateAttributePlan.qml"
		func:	"CreateAttributePlan"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Variable Sampling")
		// icon:	"analysis-bayesian-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Create Sampling Plan")
		qml:	"CreateVariablePlan.qml"
		func:	"CreateVariablePlan"
	}

	// Analysis
	// {
	// 	title:	qsTr("Accept/Reject Lots")
	// 	qml:	"VariableLots.qml"
	// 	func:	"VariableLots"
	// }
}