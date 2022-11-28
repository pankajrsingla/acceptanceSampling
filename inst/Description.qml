import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspAcceptanceSampling"
	title		: qsTr("Acceptance Sampling")
	description	: qsTr("Sampling for acceptance")
	icon		: "decision-making.png"
	version		: "0.16.4"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Attribute Sampling")
		icon:	"sample.png"
	}

	Analysis
	{
		title:	qsTr("Create Attribute Plan")
		qml:	"CreateAttributePlan.qml"
		func:	"CreateAttributePlan"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("Analyze Attribute Plan")
		qml:	"AnalyzeAttributePlan.qml"
		func:	"AnalyzeAttributePlan"
		requiresData: false
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Variable Sampling")
		icon:	"3d-printer.png"
	}

	Analysis
	{
		title:	qsTr("Create Variable Plan")
		qml:	"CreateVariablePlan.qml"
		func:	"CreateVariablePlan"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("Accept/Reject Lots")
		qml:	"DecideVariableLots.qml"
		func:	"DecideVariableLots"
		requiresData: false
	}
}