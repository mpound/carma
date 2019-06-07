-- copy this file and replace the 2012a tags with whichever semester
-- you are creating the structure for.

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

CREATE TABLE IF NOT EXISTS `2012a_proposal` (
  `proposalid` bigint(20) NOT NULL DEFAULT '0',
  `user` text COLLATE utf8_unicode_ci,
  `title` text COLLATE utf8_unicode_ci,
  `date` date DEFAULT NULL,
  `toe` tinyint(1) DEFAULT NULL,
  `priority` text COLLATE utf8_unicode_ci,
  `abstract` longtext COLLATE utf8_unicode_ci,
  `scientific_category` enum('Planetary','Solar','Stellar','High-mass Star Formation','Low-mass Star Formation','Chemistry / Interstellar Medium','Other Galactic','Galaxies - Detection','Galaxies - Mapping','Cosmology','Other Extragalactic') COLLATE utf8_unicode_ci DEFAULT NULL,
  `1cm` tinyint(1) NOT NULL DEFAULT '0',
  `3mm` tinyint(1) NOT NULL DEFAULT '0',
  `1mm` tinyint(1) NOT NULL DEFAULT '0',
  `key_project` tinyint(1) NOT NULL DEFAULT '0',
  `help_required` enum('None','Consultation','Request Collaborator') COLLATE utf8_unicode_ci DEFAULT NULL,
  `special_requirements` longtext COLLATE utf8_unicode_ci,
  `scientific_justification` longtext COLLATE utf8_unicode_ci,
  `technical_justification` longtext COLLATE utf8_unicode_ci,
  `prior_obs` longtext COLLATE utf8_unicode_ci,
  KEY `proposalid` (`proposalid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `2012a_author` (
  `proposalid` bigint(20) NOT NULL DEFAULT '0',
  `numb` bigint(20) NOT NULL DEFAULT '1',
  `name` text COLLATE utf8_unicode_ci,
  `email` text COLLATE utf8_unicode_ci,
  `phone` text COLLATE utf8_unicode_ci,
  `institution` text COLLATE utf8_unicode_ci,
  `thesis` tinyint(1) DEFAULT '0',
  `grad` tinyint(1) DEFAULT '0',
  KEY `proposalid` (`proposalid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `2012a_source` (
  `proposalid` bigint(20) NOT NULL DEFAULT '0',
  `numb` bigint(20) NOT NULL DEFAULT '1',
  `name` text COLLATE utf8_unicode_ci,
  `ra` text COLLATE utf8_unicode_ci,
  `dec` text COLLATE utf8_unicode_ci,
  `numb_fields` text COLLATE utf8_unicode_ci,
  `flexha` tinyint(1) NOT NULL DEFAULT '0',
  `imaging` enum('Imaging','SNR') COLLATE utf8_unicode_ci DEFAULT NULL,
  `species` text COLLATE utf8_unicode_ci,
  `corr_frequency` text COLLATE utf8_unicode_ci,
  `self_cal` tinyint(1) NOT NULL DEFAULT '0',
  `hrs_a` text COLLATE utf8_unicode_ci,
  `hrs_b` text COLLATE utf8_unicode_ci,
  `hrs_c` text COLLATE utf8_unicode_ci,
  `hrs_d` text COLLATE utf8_unicode_ci,
  `hrs_e` text COLLATE utf8_unicode_ci,
  `hrs_sh` text COLLATE utf8_unicode_ci,
  `hrs_sl` text COLLATE utf8_unicode_ci,
  `observation_type` text COLLATE utf8_unicode_ci NOT NULL,
  KEY `proposalid` (`proposalid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
