000001170208       // --------------------------------------------------
000009170308       // Prototype for procedure: YDRuto_INIT
000010170208       // --------------------------------------------------
000011170308       DCL-PR YDRuto_Init INT(10);
000123161206         pi_TipoOperazione char(1) const;
000125161206         kpjba LIKEDS(Kpjba) OPTIONS(*NOPASS);
000126161206       END-PR ;
000127161111
000128170208       // --------------------------------------------------
000129170308       // Prototype for procedure: YDRuto_Finalize
000130170208       // --------------------------------------------------
000131170308       DCL-PR YDRuto_Finalize INT(10);
000132170208       END-PR ;
000135170208
000136161111       // --------------------------------------------------
000137170308       // Prototype for procedure: YDRuto_NewDocumento
000138161111       // --------------------------------------------------
000139170308       DCL-PR YDRuto_NewDocumento  INT(10);
000140170117         pi_IdOperazione CHAR(10) CONST;
000141170309         pi_Id_RigaEre LIKE(ydrutoDs.Id_RigaUto) const OPTIONS(*nopass);
000144170308       END-PR ;
000159161111
000160161111       // --------------------------------------------------
000161170308       // Prototype for procedure: YDRuto_SetFattura
000162161111       // --------------------------------------------------
000163170308       DCL-PR YDRuto_SetFattura INT(10);
000164170308           PiDtFattura LIKE(ydrutoDs.DtFat) const;
000165170308           PiNrFattura LIKE(ydrutoDs.IdFat) const;
000166161212       END-PR ;
000167161111
000168161212       // --------------------------------------------------
000169170308       // Prototype for procedure: YDRuto_SetRegistrazione
000170161212       // --------------------------------------------------
000171170308       DCL-PR YDRuto_SetRegistrazione INT(10);
000172170308           PiDtRegistrazione LIKE(ydrutoDs.DtReg) const;
000173170308           PiIdRegistrazione LIKE(ydrutoDs.IdReg) const;
000174170308           PiSerieRegistrazione LIKE(ydrutoDs.SerieReg) const;
000175161212       END-PR ;
000176170308
000177170308       // --------------------------------------------------
000178170308       // Prototype for procedure: YDRuto_SetDocumento
000179170308       // --------------------------------------------------
000180170308       DCL-PR YDRuto_SetDocumento INT(10);
000181170308           PiDtBenestar LIKE(ydrutoDs.DtDoc) const;
000182170308           PiIdBenestar LIKE(ydrutoDs.IdDoc) const;
000183171220           Pi_ForzaDocumentoDoppio char(1) const options(*nopass);
000184170308       END-PR ;
000185161212
000186170310       // --------------------------------------------------
000187170310       // Prototype for procedure: YDRuto_SetIdDocFlow
000188170310       // --------------------------------------------------
000189170310       DCL-PR YDRuto_SetIdDocFlow INT(10);
000190170310           PiDocFlow LIKE(YDRutoDs.iD_DocFlow) const;
000192170310       END-PR ;
000193170310
000194161111       // --------------------------------------------------
000195170308       // Prototype for procedure: YDRuto_SetFornitore
000196161111       // --------------------------------------------------
000197170308       DCL-PR YDRuto_SetFornitore INT(10);
000198171204           PiIdFor LIKE(ydrutoDs.IdFor);
000199171204           PiCdFiscale LIKE(ydrutoDs.CFForDoc);
000200171220           PiPartitaIva LIKE(ydrutoDs.PIForDoc);
000202161114       END-PR ;
000203161111
000204161111       // --------------------------------------------------
000205170308       // Prototype for procedure: YDRuto_SetImporto
000206161111       // --------------------------------------------------
000207170308       DCL-PR YDRuto_SetImporto INT(10);
000208170309           PiImportotot LIKE(YDRutoDs.Importotot) const;
000209170309           PiImponibile LIKE(ydrutoDs.Imponib) const;
000210170308           Pidivisa LIKE(ydrutoDs.Divisa) const;
000211161111       END-PR ;
000212170308
000218161111       // --------------------------------------------------
000219170308       // Prototype for procedure: YDRuto_SetNote
000220161111       // --------------------------------------------------
000221170308       DCL-PR YDRuto_SetNote INT(10);
000222170308           PiNote LIKE(ydrutoDs.Note) const;
000223170227       END-PR ;
000229161111
000230170308       // --------------------------------------------------
000238170308       // Prototype for procedure: YDRuto_SetUtoNrAsREg
000239171220
000240161212       // --------------------------------------------------
000241170308       DCL-PR YDRuto_SetUtoNrAsReg INT(10);
000242170308           PiUtoSys LIKE(ydrutoDs.UtoSys) const;
000243170308           PiUtoNrAsReg LIKE(ydrutoDs.UtoNrAsReg) const;
000244161212       END-PR ;
000245161212
000252161111       // --------------------------------------------------
000253170308       // Prototype for procedure: YDRuto_GetMessaggi
000254161111       // --------------------------------------------------
000255170308       DCL-PR YDRuto_GetMessaggi INT(10);
000256170202         //PiMessaggi varchar(100) dim(255);
000257170203           pINrElementi int(10);
000258170202           PiIdMessaggio char(7) dim(255);
000259170202           PiTipoMessaggio char(1) dim(255);
000260170202           PiIdCampo char(10) dim(255);
000261170202           PiTestoMessaggio varchar(255) dim(255);
000262161111       END-PR ;
000263161111
000264161111       // --------------------------------------------------
000265170308       // Prototype for procedure: YDRuto_EndDocumento
000266161111       // --------------------------------------------------
000267170308       DCL-PR YDRuto_EndDocumento INT(10);
000268161205           PiFlagUpdate char(1) const;
000269161205           PiFlagCommit char(1) const;
000270171220           Pi_ForzaDocumentoDoppio char(1) const options(*nopass);
000271161111       END-PR ;
000272161111
000280161121       // --------------------------------------------------
000281170308       // Prototype for procedure: YDRuto_addMessaggio
000282161121       // --------------------------------------------------
000283170308       DCL-PR YDRuto_addMessaggio INT(10);
000284170202          // PiMessaggio varchar(255) const;
000286170202           PiIdMessaggio char(10) const;
000287170202           PiIdMsgFile char(10) const;
000288170202           PiTipoMessaggio char(1) const;
000289170202           PiIdCampo char(10) const;
000290170202           PiTestoMessaggio varchar(255) const;
000291161121       END-PR ;
000292170308
000307170130       // --------------------------------------------------
000308170308       // Prototype for procedure: YDRuto_SetUnita
000309170130       // --------------------------------------------------
000310170308       DCL-PR YDRuto_SetUnita INT(10);
000311170308           PiUnita   LIKE(ydrutoDs.Unita) const;
000312170130       END-PR ;
000313170308
000321170317       // --------------------------------------------------
000322170317       // Prototype for procedure: YDRuto_Approvazione
000323170317       // --------------------------------------------------
000324170317       DCL-PR YDRuto_Approvazione INT(10);
000325170317           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000326170317           pi_BkAmm LIKE(ydrutoDs.BkAmm) CONST;
000327170317       END-PR ;
000328170317
000329170515       // --------------------------------------------------
000330170515       // Prototype for procedure: YDRuto_ApprovazioneUndo
000331170515       // --------------------------------------------------
000332170515       DCL-PR YDRuto_ApprovazioneUndo INT(10);
000333170515           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000334171025           pi_BkAmm LIKE(ydrutoDs.BkAmm) CONST;
000335170515       END-PR ;
000336170515
000337170515       // --------------------------------------------------
000338170515       // Prototype for procedure: YDRuto_RegistraFattura
000339170515       // --------------------------------------------------
000340170515       DCL-PR YDRuto_RegistraFattura INT(10);
000341170515           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000342171018           PI_CAUSALE char(4) const;
000343171102           pi_DtReg date const;
000344170515       END-PR ;
000345170515
000346171009       // --------------------------------------------------
000347171009       // Prototype for procedure: YDRuto_Assegnazione
000348171009       // --------------------------------------------------
000349171009       DCL-PR YDRuto_Assegnazione INT(10);
000350171009           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000351171009           pi_UteAss LIKE(ydrutoDs.UteAssto) CONST;
000352171009       END-PR ;
000353171009
000354171009       // --------------------------------------------------
000355171009       // Prototype for procedure: YDRuto_BloccoPagamento
000356171009       // --------------------------------------------------
000357171009       DCL-PR YDRuto_BloccoPagamento INT(10);
000358171009           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000359171009           pi_BkAmm LIKE(ydrutoDs.BkAmm) CONST;
000360171009       END-PR ;
000361171009
000362171018       // --------------------------------------------------
000363171018       // Prototype for procedure: YDRuto_Validazione
000364171018       // --------------------------------------------------
000365171018       DCL-PR YDRuto_Validazione INT(10);
000366171018           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000367171018       END-PR ;
000368171018
000369171020       // --------------------------------------------------
000370171020       // Prototype for procedure: YDRuto_ValidazioneUndo
000371171020       // --------------------------------------------------
000372171020       DCL-PR YDRuto_ValidazioneUndo INT(10);
000373171020           pi_Id_RigaUto LIKE(ydrutoDs.Id_RigaUto) CONST;
000374171020       END-PR ;
000375171020
000376171206       // --------------------------------------------------
000377171206       // Prototype for procedure: YDRuto_SetUffIns
000378171206       // --------------------------------------------------
000379171206       DCL-PR YDRuto_SetUffIns INT(10);
000380171206           Pi_UffIns   LIKE(ydrutoDs.UffIns) const;
000381171206       END-PR ;
000382171206
