000100161206       CTL-OPT DFTACTGRP(*NO) BNDDIR('YDR');
000101161201
000102161205       DCL-DS YdrEreDs EXTNAME('YDRERE00F') QUALIFIED INZ;
000103161205       END-DS ;
000104161205
000106161205       /COPY GAITRASRC/SRCCONST,YDRERE
000107161205       /COPY GAITRASRC/SRCPROTOPR,YDRERE
000108161205
000111161205       DCL-S SkMessaggi varchar(100) DIM(255);
000113161206       DCL-S esito INT(10);
000114161213       DCL-S idrigaere INT(10);
000115170309       DCL-S nrMessaggi int(10);
000116170309       DCL-S idMessaggio char(7) dim(255);
000117170309       DCL-S tipoMessaggio char(1) dim(255);
000118170309       DCL-S idCampo char(10) dim(255);
000119170309       DCL-S testoMessaggio varchar(255) dim(255);
000120170309
000121161205       DCL-DS kpjba EXT QUALIFIED ;
000122161205       END-DS ;
000123161205
000124161201     C     *ENTRY        PLIST
000125161205     C                   PARM                    KPJBA
000126161205
000127161201       *INLR = *ON;
000128161202
000129170309       YDRere_Init(YDRERE_INSERIMENTO_AUTOMATICO : kpjba);
000130161205
000131170309       esito=YDRere_NewDocumento(YDRERE_OPERAZIONE_INSERT);
000132161201
000133170309       esito=YDRere_SetAmbito('MAGAZZINO');
000134161205
000135170109       esito=YDRere_SetArticolo('BUS0000001');
000136161205
000137161212       esito=YDRere_SetDataConsegna(%date('2016-12-25'));
000138161212
000139170309       esito=YDRere_SetDDT('132DDT':%date('2016-12-20')); // facoltativo
000140161212
000141170125       esito=YDRere_SetFornitore('0010401':'CODFISC':'01234567989');
000142161205
000143170103       esito=YDRere_SetImporto(1444:'EUR');
000144161205
000145161206       esito=YDRere_SetEntita('001':'CDC');
000146161205
000147161206       esito=YDRere_SetIdOrdine('123ORD':%date('2016-12-05'));
000148161205
000149170309       esito=YDRere_SetIdBenestare('123BEN': %date('2016-12-12')); // facoltativo
000150161205
000151170309       esito=YDRere_SetNote('piccola nota'); // descrizione materiale
000152161205
000153170309       esito=YDRere_SetIdEsterno('ARCHIlegame'); // facoltativo
000154161212
000156170309       //esito=YDRere_SetIdDocumentoRicevuto(123456789);
000157161206
000158170309       esito=YDRere_SetIdAttributo('idattributo':1); // facoltativo
000159161205
000160170309       esito=YDRere_SetPIFornitoreContestare('0133456789'); // facoltativo
000161161205
000162170309       esito=YDRere_SetIdFiliale(046); // facoltativo
000163161205
000164170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000165170309             : idCampo : testoMessaggio);
000166161206
000167161206       esito=YDRere_EndDocumento('1':'1');
000168161213       if esito > 0;
000169161213        idrigaere = esito;
000170161213       endif;
000171161206
000172170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000173170309             : idCampo : testoMessaggio);
000175161212
000176161212        // update
000177161212
000178170109
000197170309       esito=YDRere_NewDocumento(YDRERE_OPERAZIONE_UPDATE:idrigaere:*omit
000198170309        :'MAGAZZINO');
000199161213
000200170113       esito=YDRere_SetImporto(3000:'EUR');
000201170113
000202170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000203170309             : idCampo : testoMessaggio);
000204161213
000205161213       esito=YDRere_EndDocumento('1':'1');
000206161213
000207170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000208170309             : idCampo : testoMessaggio);
000210161213
000211170113        // annulla
000212170113
000213170118       esito=YDRere_NewDocumento('ANNUL':IdRigaere:*omit:'ARCHIBUS');
000214170113
000215170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000216170309             : idCampo : testoMessaggio);
000218170113
000219170113       esito=YDRere_EndDocumento('1':'1');
000220170113
000221170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000222170309             : idCampo : testoMessaggio);
000224170113
000225170113        // reset
000226170113
000227170118       esito=YDRere_NewDocumento('RESET':idrigaere:*omit:'ARCHIBUS');
000228170113
000229170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000230170309             : idCampo : testoMessaggio);
000231170113
000232170309       esito=YDRere_EndDocumento(*OFF:'1');
000233170113
000234170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000235170309             : idCampo : testoMessaggio);
000236170113
000237170113        // delete
000238170113
000239170118       esito=YDRere_NewDocumento('DELETE':idrigaere:*omit:'ARCHIBUS');
000240170113
000241170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000242170309             : idCampo : testoMessaggio);
000244170113
000245170113       esito=YDRere_EndDocumento('1':'1');
000246170113
000247170309       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
000248170309             : idCampo : testoMessaggio);
000250170118
000251161201       RETURN;
000252170109
