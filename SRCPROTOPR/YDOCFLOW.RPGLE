000002170605       DCL-DS anlgn00f EXT TEMPLATE END-DS;
000004170605
000006161209       DCL-DS docFlowMetadatiT TEMPLATE QUALIFIED INZ;
000007161201         versione CHAR(1) INZ('A');
000008161209         esportato IND;
000010161212         societa CHAR(3);
000011161209         unita CHAR(8);
000012161201         idFor CHAR(8);
000013161201         piFor VARCHAR(20);
000014161201         cfFor VARCHAR(16);
000015161209         ragFor VARCHAR(88);
000016161201         indFor VARCHAR(50);
000017161201         capFor VARCHAR(9);
000018161201         locFor VARCHAR(50);
000019161201         proFor CHAR(2);
000020161201         nazFor CHAR(4);
000021161201         dtDoc CHAR(10);
000022161201         idDoc VARCHAR(20);
000023161209         impDoc PACKED(15:2);
000024161201         dtReg CHAR(10);
000025161201         nrReg VARCHAR(9);
000026161209         serReg CHAR(4);
000027161209         uid VARCHAR(24);
000028161212         tipoEsp CHAR(1);
000029161201       END-DS;
000030160713
000104160713       DCL-PR DocFlow_IsRegistrazioneInDocFlow IND;
000106160713         piSys PACKED(3:0) CONST;
000107160713         piNrAsReg PACKED(9:0) CONST;
000108160713         piAnlgn00f LIKEDS(anlgn00f);
000109160713       END-PR ;
000110160713
000111160713       DCL-PR DocFlow_IsDocumentoInDocFlow IND;
000112160713         piSys PACKED(3:0) CONST;
000113160713         piNrAsReg PACKED(9:0) CONST;
000114160715         piCausale CHAR(4) CONST OPTIONS(*NOPASS);
000115160715         piSerie CHAR(4) CONST OPTIONS(*NOPASS);
000116160715         piSocieta CHAR(3) CONST OPTIONS(*NOPASS);
000117160713       END-PR ;
000118160713
000119160713       DCL-PR DocFlow_GetTipoSpool CHAR(4);
000120160713         piSys PACKED(3:0) CONST OPTIONS(*OMIT);
000121160713         piNrAsReg PACKED(9:0) CONST OPTIONS(*OMIT);
000122161215         piCausale CHAR(4) CONST OPTIONS(*NOPASS:*OMIT);
000123161215         piSerie CHAR(4) CONST OPTIONS(*NOPASS:*OMIT);
000124161215         piSocieta CHAR(3) CONST OPTIONS(*NOPASS:*OMIT);
000125161215         piGestioneMetadati IND OPTIONS(*NOPASS);
000126160713       END-PR ;
000127160713
000128160714       DCL-PR DocFlow_DspDocumento INT(10);
000129160714         piSys PACKED(3:0) CONST OPTIONS(*OMIT);
000130160714         piNrAsReg PACKED(9:0) CONST OPTIONS(*OMIT);
000131160714         piTipoSpool CHAR(4) CONST OPTIONS(*NOPASS);
000132160714         piSocieta CHAR(3) CONST OPTIONS(*NOPASS);
000133160714         piSerie CHAR(4) CONST OPTIONS(*NOPASS);
000134160714         piData DATE CONST OPTIONS(*NOPASS);
000135160714         piNumero PACKED(9:0) CONST OPTIONS(*NOPASS);
000136160714       END-PR ;
000137161201
000138161201       DCL-PR DocFlow_GetMetadatiEsterniByIdDocFlow LIKEDS(docFlowMetadatiT);
000139161201         piIdDocFlow VARCHAR(20) CONST;
000140161201       END-PR ;
000141161201
000142161201       DCL-PR DocFlow_GetMetadatiInterniByIdDocFlow LIKEDS(docFlowMetadatiT);
000143161201         piIdDocFlow VARCHAR(20) CONST;
000144161202         piEsportato CHAR(1) CONST OPTIONS(*NOPASS);
000145161201       END-PR ;
000146161201
000147161201       DCL-PR DocFlow_GetIdDocFlowByProjNumeroAssoluto VARCHAR(20);
000148161201         piPjSys PACKED(3:0) CONST;
000149161201         piPjNrAsReg PACKED(9:0) CONST;
000150161201       END-PR ;
000151161202
000152161202       DCL-PR DocFlow_GetIdDocFlowByIdDocumentoRicevuto VARCHAR(20);
000153161202         piIdDocumentoRicevuto INT(10) CONST;
000154161202       END-PR ;
000155161201
000156161206       DCL-PR DocFlow_InsMetadati INT(10);
000157161206         piRqsOpCode INT(10) CONST;
000158161207         piRpyOpCode INT(10);
000159161207         piRqsFormato CHAR(10) CONST;
000160161207         piRqsData CHAR(65535) CONST OPTIONS(*VARSIZE);
000161161207         piRqsDataLen INT(10) CONST;
000162161206         piMsgFormato CHAR(10) CONST OPTIONS(*NOPASS);
000163161206         piMsgData CHAR(65535) OPTIONS(*NOPASS:*VARSIZE);
000164161206         piMsgDataLen INT(10) CONST OPTIONS(*NOPASS);
000165161213         piRpyFormato CHAR(10) CONST OPTIONS(*NOPASS);
000166161213         piRpyData CHAR(65535) OPTIONS(*NOPASS:*VARSIZE);
000167161213         piRpyDataLen INT(10) CONST OPTIONS(*NOPASS);
000168161206       END-PR ;
000169161216
000170161216       DCL-PR DocFlow_RetrieveMessage CHAR(255);
000171161216         piMsgID CHAR(7) CONST;
000172161216         piReplacementData CHAR(255) CONST OPTIONS(*NOPASS:*VARSIZE);
000173161216         piLengthOfReplacementData INT(10) CONST OPTIONS(*NOPASS);
000174161216       END-PR ;
000175161216
000176170606         DCL-Pr Docflow_SetData_byStep INT(10);
000178170605            pi_id_DocFlow char(20) const;
000179170605            pi_Step char(1) const;
000180170605            pi_Societa char(3) const;
000181170605            pi_Filiale char(8) const;
000182170605            pi_IdFornitore char(8) const;
000183170605            pi_RagioneSociale varchar(88) const;
000184170605            pi_Indirizzo char(50) const;
000185170605            pi_Cap varchar(9) const;
000186170605            pi_Localita varchar(50) const;
000187170605            pi_Provincia char(2) const;
000188170605            pi_Nazione char(4) const;
000189170605            pi_CodiceFiscale varchar(16) const;
000190170605            pi_PartitaIva varchar(20) const;
000191170605            pi_NumeroFattura varchar(20) const;
000192170605            pi_DataFattura date const;
000193170605            pi_TotaleFattura packed(15:2) const;
000194170606            pi_NumeroProtocolloIVA packed(9:0) const;
000195170605            pi_DataProtocolloIva date const;
000196170605            pi_SezionaleProtocolloIva char(4) const;
000197170623            pi_Integrazione varchar(256) const;
000198170606            pi_Uid char(24) const;
000199170605         END-PR;
000200170605
000201170615       DCL-PR GetProjDocByIdDoc INT(10);
000202170615            pi_idDoc char(20) const;
000203170615            pi_NumeroDoc packed(9:0);
000204170615            pi_SerieDoc char(4);
000205170615       END-PR ;
000206170615
000207170616         DCL-Pr Docflow_SetData_byReset int(10);
000208170616            pi_id_DocFlow char(20) const;
000229170616         END-PR;
000230170616
000231170619       DCL-PR DocFlow_DspDocumentoByIdDocflow INT(10);
000232170619         piIdDocflow CHAR(20) CONST;
000235170619       END-PR ;
000240170619
000241170623       DCL-PR DocFlow_DspFascicoloByIdDocflow INT(10);
000242170623         piIdDocflow CHAR(20) CONST;
000243170623       END-PR ;
000244170623
