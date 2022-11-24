import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspAcceptanceSampling"
	title		: qsTr("Acceptance Sampling")
	description	: qsTr("Sampling for acceptance")
	icon		: "sample.png"
	version		: "0.16.4"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Attribute Sampling")
		icon:	"decision-making.png"
	}

	Analysis
	{
		title:	qsTr("Analyze Attribute Plan")
		qml:	"AnalyzeAttributePlan.qml"
		func:	"AnalyzeAttributePlan"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("Create Attribute Plan")
		qml:	"CreateAttributePlan.qml"
		func:	"CreateAttributePlan"
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

// icon attributions:
// <a href="https://www.flaticon.com/free-icons/decide" title="decide icons">Decide icons created by Freepik - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/correct" title="correct icons">Correct icons created by Uniconlabs - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/correct" title="correct icons">Correct icons created by Freepik - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/yes-no" title="yes no icons">Yes no icons created by Iconjam - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/random" title="random icons">Random icons created by Flat Icons - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/experimentation" title="experimentation icons">Experimentation icons created by Freepik - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/ruler" title="ruler icons">Ruler icons created by Freepik - Flaticon</a>
// <a href="https://www.flaticon.com/free-icons/speed" title="speed icons">Speed icons created by phatplus - Flaticon</a>