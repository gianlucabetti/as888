000001170208       // --------------------------------------------------
000009170208       // Prototype for procedure: YDRere_INIT
000010170208       // --------------------------------------------------
000011170208       DCL-PR YDRere_Init INT(10);
000123161206         pi_TipoOperazione char(1) const;
000125161206         kpjba LIKEDS(Kpjba) OPTIONS(*NOPASS);
000126161206       END-PR ;
000127161111
000128170208       // --------------------------------------------------
000129170208       // Prototype for procedure: YDRere_Finalize
000130170208       // --------------------------------------------------
000131170208       DCL-PR YDRere_Finalize INT(10);
000132170208       END-PR ;
000135170208
000136161111       // --------------------------------------------------
000137161114       // Prototype for procedure: YDRere_NewDocumento
000138161111       // --------------------------------------------------
000139161206       DCL-PR YDRere_NewDocumento  INT(10);
000140170117         pi_IdOperazione CHAR(10) CONST;
000141170117         pi_Id_RigaEre LIKE(ydrereDs.Id_RigaEre) CONST OPTIONS(*omit:*NOPASS);
000142170117         pi_IdEsterno LIKE(ydrereDs.IdEsterno) CONST OPTIONS(*omit:*NOPASS);
000143170117         PiCdAmbito LIKE(ydrereDs.cdAmbito) const OPTIONS(*omit:*NOPASS);
000144161114       END-PR ;
000145161111
000146161111       // --------------------------------------------------
000147161114       // Prototype for procedure: YDRere_SetAmbito
000148161111       // --------------------------------------------------
000149161114       DCL-PR YDRere_SetAmbito INT(10);
000150161205         PiIdAmbito LIKE(ydrereDs.cdAmbito) const;
000151161111       END-PR ;
000152161111
000153161111       // --------------------------------------------------
000154161118       // Prototype for procedure: YDRere_SetArticolo
000155161111       // --------------------------------------------------
000156161114       DCL-PR YDRere_SetArticolo INT(10);
000157161205         PiCdProdotto LIKE(ydrereDs.cdProdotto) const;
000158161111       END-PR ;
000159161111
000160161111       // --------------------------------------------------
000161161118       // Prototype for procedure: YDRere_SetDataConsegna
000162161111       // --------------------------------------------------
000163161114       DCL-PR YDRere_SetDataConsegna INT(10);
000164161212           PiDtConsegna LIKE(ydrereDs.DtConsegna) const;
000165161212       END-PR ;
000166161111
000167161212       // --------------------------------------------------
000168161212       // Prototype for procedure: YDRere_SetDataConsegna
000169161212       // --------------------------------------------------
000170161212       DCL-PR YDRere_SetDDT INT(10);
000171161212           PiDtDDT LIKE(ydrereDs.IdDDT) const;
000172161212           PiDtDDT LIKE(ydrereDs.DtDDT) const;
000173161212       END-PR ;
000174161212
000175161111       // --------------------------------------------------
000176161118       // Prototype for procedure: YDRere_SetFornitore
000177161111       // --------------------------------------------------
000178161114       DCL-PR YDRere_SetFornitore INT(10);
000179161205           PiIdFor LIKE(ydrereDs.IdFor) const;
000180161205           PiCdFiscale LIKE(ydrereDs.CFForDoc) const;
000181161205           PiPartitaIva LIKE(ydrereDs.PIvaFor) const;
000182161114       END-PR ;
000183161111
000184161111       // --------------------------------------------------
000185161118       // Prototype for procedure: YDRere_SetImporto
000186161111       // --------------------------------------------------
000187161205       DCL-PR YDRere_SetImporto INT(10);
000188161205           PiImponibile LIKE(ydrereDs.Imponibile) const;
000189161205           Pidivisa LIKE(ydrereDs.Divisa) const;
000190161111       END-PR ;
000191161111
000192161111       // --------------------------------------------------
000193161118       // Prototype for procedure: YDRere_SetEntita
000194161111       // --------------------------------------------------
000195161205       DCL-PR YDRere_SetEntita INT(10);
000196161205           PiEntita LIKE(ydrereDs.IdCDC) const;
000197161205           PiTipoEntita char(3) const;
000198161111       END-PR ;
000199161111
000200161111       // --------------------------------------------------
000201161118       // Prototype for procedure: YDRere_SetIdOrdine
000202161111       // --------------------------------------------------
000203161114       DCL-PR YDRere_SetIdOrdine INT(10);
000204161205           PiIdOrdine LIKE(ydrereDs.IdOrdine) const;
000205161205           PiDtOrdine LIKE(ydrereDs.DtOrdine) const;
000206161111       END-PR ;
000207161111
000208161111       // --------------------------------------------------
000209161118       // Prototype for procedure: YDRere_SetIdBenestare
000210161111       // --------------------------------------------------
000211161114       DCL-PR YDRere_SetIdBenestare INT(10);
000212161205           PiIdBenestar LIKE(ydrereDs.IdBenestar) const;
000213161205           PiDtBenestar LIKE(ydrereDs.DtBenestar) const;
000214161111       END-PR ;
000215161111
000216161111       // --------------------------------------------------
000217161118       // Prototype for procedure: YDRere_SetNote
000218161111       // --------------------------------------------------
000219161114       DCL-PR YDRere_SetNote INT(10);
000220161205           PiNote LIKE(ydrereDs.Note) const;
000221170227       END-PR ;
000229161111
000230161212       // --------------------------------------------------
000231161212       // Prototype for procedure: YDRere_SetIdRigaUto
000232161212       // --------------------------------------------------
000233161212       DCL-PR YDRere_SetIdEsterno INT(10);
000234161212           PiIdEsterno LIKE(ydrereDs.IdEsterno) const;
000235161212       END-PR ;
000236161212
000237161212       // --------------------------------------------------
000238161212       // Prototype for procedure: YDRere_SetEreNrAsREg
000239161212       // --------------------------------------------------
000240161212       DCL-PR YDRere_SetereNrAsReg INT(10);
000241161212           PiEreSys LIKE(ydrereDs.EreSys) const;
000242161212           PiEreNrAsReg LIKE(ydrereDs.EreNrAsReg) const;
000243161212       END-PR ;
000244161212
000252161111       // --------------------------------------------------
000253161118       // Prototype for procedure: YDRere_GetMessaggi
000254161111       // --------------------------------------------------
000255161114       DCL-PR YDRere_GetMessaggi INT(10);
000256170202         //PiMessaggi varchar(100) dim(255);
000257170203           pINrElementi int(10);
000258170202           PiIdMessaggio char(7) dim(255);
000259170202           PiTipoMessaggio char(1) dim(255);
000260170202           PiIdCampo char(10) dim(255);
000261170202           PiTestoMessaggio varchar(255) dim(255);
000262161111       END-PR ;
000263161111
000264161111       // --------------------------------------------------
000265161118       // Prototype for procedure: YDRere_EndDocumento
000266161111       // --------------------------------------------------
000267161114       DCL-PR YDRere_EndDocumento INT(10);
000268161205           PiFlagUpdate char(1) const;
000269161205           PiFlagCommit char(1) const;
000270161111       END-PR ;
000271161111
000272161118       // --------------------------------------------------
000273161118       // Prototype for procedure: YDRere_SetIdAttributo
000274161118       // --------------------------------------------------
000275161118       DCL-PR YDRere_SetIdAttributo INT(10);
000276161205           PiIdAttributo LIKE(ydrereDs.Attributo1) const;
000277161205           PiNrAttributo packed(2:0) const;
000278161118       END-PR ;
000279161205
000280161121       // --------------------------------------------------
000281161205       // Prototype for procedure: YDRere_addMessaggio
000282161121       // --------------------------------------------------
000283161121       DCL-PR YDRere_addMessaggio INT(10);
000284170202          // PiMessaggio varchar(255) const;
000286170202           PiIdMessaggio char(10) const;
000287170202           PiIdMsgFile char(10) const;
000288170202           PiTipoMessaggio char(1) const;
000289170202           PiIdCampo char(10) const;
000290170202           PiTestoMessaggio varchar(255) const;
000291161121       END-PR ;
000292161121
000293161205       // --------------------------------------------------
000294170210       // Prototype for procedure: YDRere_SetPIFornitoreContestare
000295161205       // --------------------------------------------------
000296161205       DCL-PR YDRere_SetPIFornitoreContestare INT(10);
000297161205           PiPIFornitoreContestare LIKE(ydrereDs.PIvaForCon) const;
000298161205       END-PR ;
000299161205
000300161205       // --------------------------------------------------
000301170210       // Prototype for procedure: YDRere_SetIdFiliale
000302161205       // --------------------------------------------------
000303161205       DCL-PR YDRere_SetIdFiliale INT(10);
000304161205           PiIdFiliale LIKE(ydrereDs.IdFiliale) const;
000305161205       END-PR ;
000306161205
000307170130       // --------------------------------------------------
000308170210       // Prototype for procedure: YDRere_SetUnita
000309170130       // --------------------------------------------------
000310170130       DCL-PR YDRere_SetUnita INT(10);
000311170130           PiUnita   LIKE(ydrereDs.Unita) const;
000312170130       END-PR ;
000313170130
000314170210       // --------------------------------------------------
000315170210       // Prototype for procedure: YDRere_SetFlagRecordSelezionato
000316170210       // --------------------------------------------------
000317170210       DCL-PR YDRere_SetFlagRecordSelezionato INT(10);
000318170210           PiFlagSelez LIKE(ydrereDs.FlagSelez) const;
000319170227           PiIdRigaUto LIKE(ydrereDs.IdRigaUto) const;
000320170210       END-PR ;
000321170210
000322170210       // --------------------------------------------------
000323170210       // Prototype for procedure: YDRere_DocumentoSelezionato
000324170210       // --------------------------------------------------
000325170210       DCL-PR YDRere_DocumentoSelezionato INT(10);
000326170210         pi_Id_RigaEre LIKE(ydrereDs.Id_RigaEre) CONST;
000327170210         pi_Id_RigaEre LIKE(ydrereDs.flagSelez) CONST;
000328170210       END-PR ;
000329170210
000330170210       // --------------------------------------------------
000331170210       // Prototype for procedure: YDRere_DocumentoRicevuto
000332170210       // --------------------------------------------------
000333170210       DCL-PR YDRere_DocumentoRicevuto INT(10);
000334170210         pi_Id_RigaEre LIKE(ydrereDs.Id_RigaEre) CONST;
000335170210       END-PR ;
000336170210
000337170214       // --------------------------------------------------
000338170214       // Prototype for procedure: YDRere_IsIdEsternoUsed
000339170214       // --------------------------------------------------
000340170214       DCL-PR YDRere_IsIdEsternoUsed IND;
000341170214           pi_cdambito like(ydrereds.cdambito) const;
000342170214           pi_idesterno like(ydrereds.idesterno) const;
000343170214       END-PR ;
000345170214
