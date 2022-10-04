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
		title:	qsTr("Attribute Plans")
		// icon:	"analysis-classical-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("OC Curves")
		func:	"OCCurves"
	}

	Analysis
	{
		title:	qsTr("Assess Sampling Plan")
		func:	"AssessSamplingPlan"
	}
	
	Analysis
	{
		title:	qsTr("Find Sampling Plan")
		func:	"FindSamplingPlan"
	}

	// Separator {}

	// GroupTitle
	// {
	// 	title:	qsTr("Variable Plans")
	// 	icon:	"analysis-bayesian-crosstabs.svg"
	// }
}