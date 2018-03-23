000001170208       // --------------------------------------------------
000009170309       // Prototype for procedure: YDRere_INIT
000010170208       // --------------------------------------------------
000011170309       DCL-PR YDRere_Init INT(10);
000123161206         pi_TipoOperazione char(1) const;
000125161206         kpjba LIKEDS(Kpjba) OPTIONS(*NOPASS);
000126161206       END-PR ;
000127161111
000128170208       // --------------------------------------------------
000129170309       // Prototype for procedure: YDRere_Finalize
000130170208       // --------------------------------------------------
000131170309       DCL-PR YDRere_Finalize INT(10);
000132170208       END-PR ;
000135170208
000136161111       // --------------------------------------------------
000137170309       // Prototype for procedure: YDRere_NewDocumento
000138161111       // --------------------------------------------------
000139170309       DCL-PR YDRere_NewDocumento  INT(10);
000140170117         pi_IdOperazione CHAR(10) CONST;
000141170117         pi_Id_RigaEre LIKE(ydrereDs.Id_RigaEre) CONST OPTIONS(*omit:*NOPASS);
000142170117         pi_IdEsterno LIKE(ydrereDs.IdEsterno) CONST OPTIONS(*omit:*NOPASS);
000143170117         PiCdAmbito LIKE(ydrereDs.cdAmbito) const OPTIONS(*omit:*NOPASS);
000144161114       END-PR ;
000145161111
000146161111       // --------------------------------------------------
000147170309       // Prototype for procedure: YDRere_SetAmbito
000148161111       // --------------------------------------------------
000149170309       DCL-PR YDRere_SetAmbito INT(10);
000150161205         PiIdAmbito LIKE(ydrereDs.cdAmbito) const;
000151161111       END-PR ;
000152161111
000153161111       // --------------------------------------------------
000154170309       // Prototype for procedure: YDRere_SetArticolo
000155161111       // --------------------------------------------------
000156170309       DCL-PR YDRere_SetArticolo INT(10);
000157161205         PiCdProdotto LIKE(ydrereDs.cdProdotto) const;
000158161111       END-PR ;
000159161111
000160161111       // --------------------------------------------------
000161170309       // Prototype for procedure: YDRere_SetDataConsegna
000162161111       // --------------------------------------------------
000163170309       DCL-PR YDRere_SetDataConsegna INT(10);
000164161212           PiDtConsegna LIKE(ydrereDs.DtConsegna) const;
000165161212       END-PR ;
000166161111
000167170322       // --------------------------------------------------
000168170322       // Prototype for procedure: YDRere_SetDDT
000169170322       // --------------------------------------------------
000170170322       DCL-PR YDRere_SetDDT INT(10);
000171170322           PiDtDDT LIKE(ydrereDs.IdDDT) const;
000172170322           PiDtDDT LIKE(ydrereDs.DtDDT) const;
000174170322       END-PR ;
000175170322
000176170322       // --------------------------------------------------
000177170322       // Prototype for procedure: YDRere_SetFAT
000178170322       // --------------------------------------------------
000179170322       DCL-PR YDRere_SetFAT INT(10);
000180170322           PiDtFAT LIKE(ydrereDs.IdFAT) const;
000181170322           PiDtFAT LIKE(ydrereDs.DtFAT) const;
000182170322       END-PR ;
000183170322
000184161111       // --------------------------------------------------
000185170309       // Prototype for procedure: YDRere_SetFornitore
000186161111       // --------------------------------------------------
000187170309       DCL-PR YDRere_SetFornitore INT(10);
000188171204           PiIdFor LIKE(ydrereDs.IdFor) ;
000189171204           PiCdFiscale LIKE(ydrereDs.CFForDoc);
000190171204           PiPartitaIva LIKE(ydrereDs.PIvaFor);
000191161114       END-PR ;
000192161111
000193161111       // --------------------------------------------------
000194170309       // Prototype for procedure: YDRere_SetImporto
000195161111       // --------------------------------------------------
000196170309       DCL-PR YDRere_SetImporto INT(10);
000197171204           PiImponibile LIKE(ydrereDs.Imponibile) const;
000198171204           Pidivisa LIKE(ydrereDs.Divisa) const;
000199161111       END-PR ;
000200161111
000201161111       // --------------------------------------------------
000202170309       // Prototype for procedure: YDRere_SetEntita
000203161111       // --------------------------------------------------
000204170309       DCL-PR YDRere_SetEntita INT(10);
000205161205           PiEntita LIKE(ydrereDs.IdCDC) const;
000206161205           PiTipoEntita char(3) const;
000207161111       END-PR ;
000208161111
000209161111       // --------------------------------------------------
000210170309       // Prototype for procedure: YDRere_SetIdOrdine
000211161111       // --------------------------------------------------
000212170407       DCL-PR YDRere_SetOrdine INT(10);
000213161205           PiIdOrdine LIKE(ydrereDs.IdOrdine) const;
000214161205           PiDtOrdine LIKE(ydrereDs.DtOrdine) const;
000215161111       END-PR ;
000216161111
000217161111       // --------------------------------------------------
000218170309       // Prototype for procedure: YDRere_SetIdBenestare
000219161111       // --------------------------------------------------
000220170407       DCL-PR YDRere_SetBenestare INT(10);
000221161205           PiIdBenestar LIKE(ydrereDs.IdBenestar) const;
000222161205           PiDtBenestar LIKE(ydrereDs.DtBenestar) const;
000223161111       END-PR ;
000224161111
000225161111       // --------------------------------------------------
000226170309       // Prototype for procedure: YDRere_SetNote
000227161111       // --------------------------------------------------
000228170309       DCL-PR YDRere_SetNote INT(10);
000229161205           PiNote LIKE(ydrereDs.Note) const;
000230170227       END-PR ;
000231161111
000232161212       // --------------------------------------------------
000233170309       // Prototype for procedure: YDRere_SetIdRigaUto
000234161212       // --------------------------------------------------
000235170309       DCL-PR YDRere_SetIdEsterno INT(10);
000236161212           PiIdEsterno LIKE(ydrereDs.IdEsterno) const;
000237161212       END-PR ;
000238161212
000239161212       // --------------------------------------------------
000240170309       // Prototype for procedure: YDRere_SetEreNrAsREg
000241161212       // --------------------------------------------------
000242170309       DCL-PR YDRere_SetereNrAsReg INT(10);
000243161212           PiEreSys LIKE(ydrereDs.EreSys) const;
000244161212           PiEreNrAsReg LIKE(ydrereDs.EreNrAsReg) const;
000245161212       END-PR ;
000246161212
000252161111       // --------------------------------------------------
000253170309       // Prototype for procedure: YDRere_GetMessaggi
000254161111       // --------------------------------------------------
000255170309       DCL-PR YDRere_GetMessaggi INT(10);
000256170202         //PiMessaggi varchar(100) dim(255);
000257170203           pINrElementi int(10);
000258170202           PiIdMessaggio char(7) dim(255);
000259170202           PiTipoMessaggio char(1) dim(255);
000260170202           PiIdCampo char(10) dim(255);
000261170202           PiTestoMessaggio varchar(255) dim(255);
000262161111       END-PR ;
000263161111
000264161111       // --------------------------------------------------
000265170309       // Prototype for procedure: YDRere_EndDocumento
000266161111       // --------------------------------------------------
000267170309       DCL-PR YDRere_EndDocumento INT(10);
000268161205           PiFlagUpdate char(1) const;
000269161205           PiFlagCommit char(1) const;
000270161111       END-PR ;
000271161111
000272161118       // --------------------------------------------------
000273170309       // Prototype for procedure: YDRere_SetIdAttributo
000274161118       // --------------------------------------------------
000275170309       DCL-PR YDRere_SetIdAttributo INT(10);
000276161205           PiIdAttributo LIKE(ydrereDs.Attributo1) const;
000277161205           PiNrAttributo packed(2:0) const;
000278161118       END-PR ;
000279161205
000280161121       // --------------------------------------------------
000281170309       // Prototype for procedure: YDRere_addMessaggio
000282161121       // --------------------------------------------------
000283170309       DCL-PR YDRere_addMessaggio INT(10);
000284170202          // PiMessaggio varchar(255) const;
000286170202           PiIdMessaggio char(10) const;
000287170202           PiIdMsgFile char(10) const;
000288170202           PiTipoMessaggio char(1) const;
000289170202           PiIdCampo char(10) const;
000290170202           PiTestoMessaggio varchar(255) const;
000291161121       END-PR ;
000292161121
000293161205       // --------------------------------------------------
000294170309       // Prototype for procedure: YDRere_SetPIFornitoreContestare
000295161205       // --------------------------------------------------
000296170309       DCL-PR YDRere_SetPIFornitoreContestare INT(10);
000297161205           PiPIFornitoreContestare LIKE(ydrereDs.PIvaForCon) const;
000298161205       END-PR ;
000299161205
000300161205       // --------------------------------------------------
000301170309       // Prototype for procedure: YDRere_SetIdFiliale
000302161205       // --------------------------------------------------
000303170309       DCL-PR YDRere_SetIdFiliale INT(10);
000304161205           PiIdFiliale LIKE(ydrereDs.IdFiliale) const;
000305161205       END-PR ;
000306161205
000307170130       // --------------------------------------------------
000308170309       // Prototype for procedure: YDRere_SetUnita
000309170130       // --------------------------------------------------
000310170309       DCL-PR YDRere_SetUnita INT(10);
000311170130           PiUnita   LIKE(ydrereDs.Unita) const;
000312170130       END-PR ;
000313170130
000314170210       // --------------------------------------------------
000315170309       // Prototype for procedure: YDRere_SetFlagRecordSelezionato
000316170210       // --------------------------------------------------
000317170309       DCL-PR YDRere_SetFlagRecordSelezionato INT(10);
000318170210           PiFlagSelez LIKE(ydrereDs.FlagSelez) const;
000319170227           PiIdRigaUto LIKE(ydrereDs.IdRigaUto) const;
000320170210       END-PR ;
000321170210
000322170210       // --------------------------------------------------
000323170309       // Prototype for procedure: YDRere_DocumentoSelezionato
000324170210       // --------------------------------------------------
000325170309       DCL-PR YDRere_DocumentoSelezionato INT(10);
000326170210         pi_Id_RigaEre LIKE(ydrereDs.Id_RigaEre) CONST;
000327170313         pi_IdRigaUto LIKE(ydrereDs.IdRigaUto) CONST;
000328170210       END-PR ;
000329170210
000330170313       // --------------------------------------------------
000331170313       // Prototype for procedure: YDRere_AssociaDocumentiSelezionati
000332170313       // --------------------------------------------------
000333170313       DCL-PR YDRere_AssociaDocumentiSelezionati INT(10);
000334170313           pi_IdRigaUto LIKE(ydrereDs.IdRigaUto) CONST;
000335170313       END-PR ;
000336170316
000344170214       // --------------------------------------------------
000345170309       // Prototype for procedure: YDRere_IsIdEsternoUsed
000346170214       // --------------------------------------------------
000347170309       DCL-PR YDRere_IsIdEsternoUsed IND;
000348170214           pi_cdambito like(ydrereds.cdambito) const;
000349170214           pi_idesterno like(ydrereds.idesterno) const;
000350170214       END-PR ;
000351170214
000352170410       // --------------------------------------------------
000353170410       // Prototype for procedure: YDRere_GetFornitore
000354170410       // --------------------------------------------------
000355170410       DCL-PR YDRere_GetFornitore INT(10);
000356170410           PiIdFor LIKE(ydrereDs.IdFor);
000357170411           PiPartitaIva LIKE(ydrereDs.PIvaFor) const;
000359170411           PiCdFiscale LIKE(ydrereDs.CFForDoc) const;
000360170411       END-PR ;
000361170410
000362170626       // --------------------------------------------------
000363170626       // Prototype for procedure: YDRere_DisassociaDocumenti
000364170626       // --------------------------------------------------
000365170626       DCL-PR YDRere_DisassociaDocumenti INT(10);
000366170626           pi_IdRigaUto LIKE(ydrereDs.IdRigaUto) CONST;
000367170626       END-PR ;
000368170626
