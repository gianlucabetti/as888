000102160804       ctl-opt actgrp(*new);
000103160804
000104160804       /copy gaitrasrc/srcprotopr,tis778r
000105160804       /copy gaitrasrc/srcconst,tis778r
000106160804
000108160804       dcl-s rpyOpCode int(10);
000109160804       dcl-s rpyIdMsg int(10);
000110160804       dcl-ds tis778dsi ext qualified inz(*extdft) end-ds;
000111160804       dcl-ds tis778dso ext qualified inz(*extdft) end-ds;
000112160804
000113160804       *inlr = *on;
000114160804
000115160804       exec sql
000116160804         declare tas no scroll cursor for
000117160804         select digits(tasLnp) concat digits(tasNrs) concat digits(tasNsp)
000118160804           from titas00f
000119160804           where tasAas = 2016 and tasLnp = 1
000120160804       ;
000121160804
000122160804       exec sql open tas;
000123160804
000124160804       if sqlCode < *zero;
000125160804         dump(a);
000126160804         return;
000127160804       endif;
000128160804
000129160804       dou sqlCode < *zero;
000130160804         exec sql fetch next from tas into :tis778dsi.nspedizI74;
000131160804         select;
000132160804           when sqlCode = 100;
000133160804             leave;
000134160804           when sqlCode < *zero;
000135160804             dump(a);
000136160804             leave;
000137160804         endsl;
000140160804
000141160804         MONITOR;
000142160804           TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
000143160804                          : rpyOpCode
000144160804                          : rpyIdMsg
000145160804                          : TIS778DSI.formatoi74
000146160804                          : TIS778DSI
000147160804                          : %SIZE(TIS778DSI)
000148160804                          : TIS778DSO.formatoo74
000149160804                          : TIS778DSO
000150160804                          : %SIZE(TIS778DSO)
000154160804                          );
000155160804           ON-ERROR;
000156160804             dump(a);
000157160804         ENDMON;
000158160804
000159160804         if rpyOpCode = *zero and tis778dso.rifero74 = *zero;
000160160804           dump(a);
000161160804         ENDIF;
000162160804
000165160804       enddo;
000166160804
000167160804       exec sql close tas;
