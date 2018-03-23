000102161130       // --------------------------------------------------
000103161130       // Prototype for procedure: SendMessage
000104161130       // --------------------------------------------------
000105161130       DCL-PR SendMessage EXTPGM('QEZSNDMG') ;
000106161130         messageType CHAR(10) CONST;
000107161130         deliveryMode CHAR(10) CONST;
000108161130         messageText CHAR(494) CONST OPTIONS(*VARSIZE);
000109161130         lengthOfmessageText INT(10) CONST;
000110161130         listOfUserProfileOrDisplayStationNames CHAR(10) DIM(299) CONST
000112161130              OPTIONS(*VARSIZE);
000113161130         numberOfUserProfileOrDisplayStationNames INT(10) CONST;
000114161130         messageSentIndicator INT(10);
000115161130         functionRequested INT(10);
000116161130         errorCode CHAR(32767) OPTIONS(*VARSIZE);
000117161130         // Optional Parameter Group 2:
000118161130         showSendAMessageDisplay CHAR(1) CONST OPTIONS(*NOPASS);
000119161130         qualifiedMessageQueueName CHAR(20) CONST OPTIONS(*NOPASS);
000120161130         nameTypeIndicator CHAR(4) CONST OPTIONS(*NOPASS);
000121161130         // Optional Parameter Group 3:
000122161130         codedCharacterSetIdentifier INT(10) CONST OPTIONS(*NOPASS);
000123161130       END-PR ;
