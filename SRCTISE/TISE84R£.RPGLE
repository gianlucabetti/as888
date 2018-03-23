000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050704      * ?------------------------------------------------------------------ */
000300050704      * ? Deve leggere le bolle saltando le spedizioni presenti su TITAS00F
000400050704      * ? poichè non ancora fatturate. (IGNORE del record)
000500050704      * ? Le letture saranno su 12 mesi e solo di Bolle Export legate ai Network
000600050704      * ? selezionati.
000700050704      * ? Non dovranno essere prese in considerazione le bolle STORNATE in base
000800050704      * ? al tipo Bolla oppure devono essere ignorate quelle con NOTE di CREDITO.
000900050705      *
001000050705     ?* ***  Regola del Delta
001100050705     ?* ***     (RICAVO - COMPETENZE) = GAP
001200050705     ?* ***     (GAP x 100) : RICAVO  = DELTA PERCENTUALE
001300050704      * ?
001400050510      **********************************************************************
001500050704     ftitas38C  if   E           K DISK
001600050704     F                                     IGNORE(TITAS000)
001700050705     FTita730C  iF   E           k DISK
001800050711     FECCET30C  iF   E           k DISK
001900050711     FECCED30C  iF   E           k DISK
002000050704     ftabel00f  if   E           K DISK
002100050513     fazorg01l  if   E           K DISK
002200140612     fAZCMM01L  if   E           K DISK    USROPN
002300050705     ftntam01l  if   E           K DISK
002400050513      **
002500050712     fwfcex01l  uf a E           K DISK
002600050704      * ?------------------------------------------------------------------ */
002700040820     d KPJBA         E DS
002800050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900110511     D CNCR42        E DS
003000050513     D*------------------
003100050513     D* DS REPERIMENTO DATI UTENTE
003200050513     D*-------------------
003300050513     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003400050513     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003500050513     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003600050513     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003700050513     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003800050513     D*------------------
003900050513     D* DS "TIBS10R"
004000050513     D*------------------
004100050513     D BS10DS        E DS                  EXTNAME(TIBS10DS)
004200050705     D*------------------
004300050705     D* DS TIPI BOLLA
004400050705     D*------------------
004500050705     D DSTB          E DS
004600050705     D TBL             S              2    DIM(30)
004700050705     D RBL             S              1    DIM(30)
004800050705     D TPO             S              1    DIM(30)
004900050517      *------------------
005000050517     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
005100050517     D*------------------
005200050517     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005300050517     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
005400050517     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005500050517     D*------------------
005600050517     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
005700050517     D*------------------
005800050517     D DS2W          E DS
005900050718     D*------------------
006000050718     D* DS Settore Merceologico
006100050718     D*------------------
006200050718     D DS1L          E DS
006300050718     D SME             S              5    DIM(200)                             *Cod.SMErceologico
006400050718     D DSME            S             50    DIM(200)                             *Des.SMErceologico
006500050718     D*------------------
006600050718     D* DS Nazione
006700050718     D*------------------
006800050718     D DS15          E DS
006900050718     D NAZ             S              4    DIM(300)                             *Cod.Nazione
007000050718     D DNAZ            S             25    DIM(300)                             *Des.Nazione
007100050517     D*------------------
007200050704     D DsPassaggio   e DS                  EXTNAME(Tise83ds)
007300050513     D*------------------
007400040820     D WLBDAT          DS
007500040820     D  G02DAT                 1      8  0
007600040820     D  G02INV                 9     16  0
007700040820     D  G02ERR                17     17
007800040820     D  G02TGI                18     22  0
007900050513     D*------------------
008000050513     D* ORGANIGRAMMA
008100050513     D*------------------
008200050704     D NTWdpd          S              3
008300050704     D NTWeex          S              3
008400050704     D NTWfed          S              3
008500050704     D Og143         e ds
008600050705     D*------------------
008700050705     D* Tariffe
008800050705     D*------------------
008900050705     D DSTA01        e ds
009000050704     D*------------------
009100050704     D FILN            S              3  0 DIM(500)                             *FILIALI x NTW
009200050513     D FIL             S              3  0 DIM(500)                             *FILIALI
009300050513     D DFIL            S             20    DIM(500)
009400050704     D NTW             S              3    DIM(500)                             *NETWORK
009500050513     D ARE             S              3  0 DIM(500)                             *AREE
009600050513     D DARE            S             20    DIM(500)
009700050513     D DIV             S              1    DIM(500)                             *DIVISIONI
009800050513     D DDIV            S             20    DIM(500)
009900050705     D*------------------
010000050705     D* COMMERCIALI
010100050705     D*------------------
010200140612     D***** ds01          e ds
010300100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
010400100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
010500100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
010600050705     D*------------------
010700040820     D  data_Oggi      s                   like(G02INV) INZ(0)
010800040826     D  almeno_uno     s              1
010900050706     D  rec_x_cliente  s              1
011000050517     D  reck_OK        s              1
011100050705     D*------------------
011200050706     d tasANNOme       ds
011300050706     D  TasAAS
011400050706     D  TasMGS
011500050706      *
011600050706     d FinoA           ds
011700050706     D  AnnoFIN                       4s 0 inz(0)
011800050706     D  MeseGGfin                     4s 0 inz(0)
011900050704      * ?------------------------------------------------------------------ */
012000050704     D  savAAS         s                   like(tasAAS)
012100050704     D  savMGS         s                   like(tasMGS)
012200050704     D  savKSC         s                   like(tasKSC)
012300050704      * ?------------------------------------------------------------------ */
012400050705     d DS_Inp_Key      ds
012500050708     D  INP_ANN                            like(cexANN)
012600050708     D  INP_MES                            like(cexMES)
012700050705     D  INP_div                            like(cexDIV)
012800050705     D  INP_did                            like(cexDID)
012900050705     D  INP_are                            like(cexARE)
013000050705     D  INP_ard                            like(cexARD)
013100050705     D  INP_fil                            like(cexFIL)
013200050705     D  INP_fid                            like(cexFID)
013300050705     D  INP_age                            like(cexAGE)
013400050705     D  INP_rgf                            like(cexAGE)
013500050705     D  INP_agd                            like(cexAGD)
013600050705     D  INP_KSC                            like(cexKSC)
013700050705     D  INP_cld                            like(cexCLD)
013800050705     D  INP_clv                            like(cexCLV)
013900050708     D  INP_dps                            like(cexDAT)
014000050718     D  INP_sme                            like(cexSME)
014100050718     D  INP_dsm                            like(cexDSM)
014200050705     D  INP_KSU                            like(cexKSU)
014300050705     D  INP_dks                            like(cexDKS)
014400050705     D  INP_NTW                            like(cexNTW)
014500050705     D  INP_NOT                            like(cexNOT)
014600050705     D  INP_NAZ                            like(cexNAZ)
014700050718     D  INP_DNA                            like(cexDNA)
014800050705     D  INP_FA                             like(cexFA)
014900050707     D  INP_DIN                            like(cexDIN)
015000050707     D  INP_DFI                            like(cexDFI)
015100050707      *
015200050706     D  INP_DAT        s              8  0
015300050705     D  INP_RBL        s              1
015400050705      *
015500050705     d DS_Inp_Dati     ds
015600050705     D  INP_TSPED                          like(cexTSPED)
015700050705     D  INP_TCOLLI                         like(cexTCOLLI)
015800050705     D  INP_TVOL                           like(cexTVOL)
015900050705     D  INP_TPES                           like(cexTPES)
016000050705     D  INP_TFAT                           like(cexTFAT)
016100090923     D  INP_TCMP                           like(cetpar)
016200090923     D  TCMPs                              like(cexTCMP)
016300050707      *
016400050707     D  INP_DAN        s                   like(cexDAN)
016500050707     D  INP_DME        s                   like(cexDME)
016600050708     D  INP_AAN        s                   like(cexAAN)
016700050708     D  INP_AME        s                   like(cexAME)
016800050704      * ?------------------------------------------------------------------ */
016900050511     D* Reperimento nome PGM
017000050511     D STATUS         SDS           333
017100050511     D  DSPGM            *PROC
017200050511     D*-------------
017300040820     D digits          C                   CONST('0123456789')
017400050705      *---------------------------------------------------------------------*
017500050711     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCET30C
017600050711     IECCET000      03
017700050711     IECCETP00      04
017800050711     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCED30C
017900050711     IECCED000      05
018000050711     IECCEDP00      06
018100050704      * ?------------------------------------------------------------------ */
018200050513      **  Il File è organizzato x Cliente
018300050511      * ?------------------------------------------------------------------ */
018400050704     c                   clear                   almeno_uno
018500050704      **
018600050705     C     TAS_INI       setll     TITAS38C                                     AAS/MGS/KSC
018700050704     C                   READ      TITAS38C
018800050704      **
018900050704     c                   dow       not %Eof(TITAS38C)
019000050704      **
019100050704      **  Fino all'Anno/Mese
019200050706     c                   if        TasANNOMe > FINOA
019300050704     c                   leave
019400050704     c                   end
019500050704      **
019600050704     c                   z-add     tasAAS        savAAS
019700050705     c                   z-add     tasMGS        savMGS
019800050704     c                   z-add     tasKSC        savKSC
019900050705      **
020000050705      *  Pulisce DS dove preimpostare i campi di Input di scrittura/Update
020100050705      *   del record.
020200050705     c                   clear                   DS_Inp_Key
020300050706     C                   clear                   rec_x_cliente
020400050704      **
020500050704      * ? x rottura di Codice Cliente (Pulizia campi x Totale Cliente)
020600050704      * ?              /*---------------------- */
020700050704     c                   eXSr      NEW_Cliente
020800050704      * ?              /*---------------------- */
020900050704      **
021000050704      **  Legge x ANNO/MESE/CLIENTE
021100050704     c                   dow       not %Eof(TITAS38C)  and
021200050704     c                                tasAAS = savAAS  and
021300050704     c                                tasMGS = savMGS  and
021400050704     c                                tasKSC = savKSC
021500050705      * ?              /*---------------------- */
021600050705     c                   eXSr      riga_x_riga
021700050705      * ?              /*---------------------- */
021800050704      **
021900050704     C                   READ      TITAS38C
022000050704     c                   enddo
022100050704      **
022200050704      **  Totali x Cliente Anno/Mese
022300050704      * ?              /*---------------------- */
022400050704     c                   exsr      tot_Cliente
022500050704      * ?              /*---------------------- */
022600050704     c                   enddo
022700050513      ** Totali
022800050704      * ?              /*---------------------- */
022900050704     c                   exsr      tot_Fine
023000050704      * ?              /*---------------------- */
023100050705      **
023200050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
023300041007     c     fine          tag
023400020326     C                   SETON                                        LR
023500050705     ?****************************************************************************
023600050705      *?   Dettaglio di ogni BOLLA
023700050705     ?****************************************************************************
023800050705     c     riga_x_riga   begsr
023900050705      *
024000050705      * controllo riga se da prendere
024100050705      * ?              /*---------------------- */
024200050705     c                   eXSr      Chk_Bolla
024300050705      * ?              /*---------------------- */
024400050705      **
024500050705      ** Se il record è da prendere in considerazione
024600050705     c                   if        reck_OK = 'S'
024700050705     c                   clear                   DS_Inp_DATI
024800050706     C                   eval      rec_x_cliente = 'S'
024900050705      * ?              /*---------------------- */
025000050705     c                   eXSr      Decod_Bolla
025100050705      * ?              /*---------------------- */
025200050705     c                   eXSr      WRI_rig_det
025300050705      * ?              /*---------------------- */
025400050705     c                   move      'S'           almeno_uno
025500050705     c                   end
025600050705      *
025700050705     c                   endsr
025800050705     ?****************************************************************************
025900050705      *?   A Rottura di Codice Cliente - Operazioni a Totale Cliente Precedente
026000050705     ?****************************************************************************
026100050705     c     NEW_Cliente   begsr
026200050705      *
026300131008      * ? Decodifiche Anagrafiche
026400050705     c                   z-add     tasKSC        inp_KSC
026500050708     c                   z-add     tasAAS        inp_ANN
026600050708     c                   movel     tasMGS        inp_MES
026700131008      **
026800131008      *?   Decodifiche ANAGRAFICHE/Tabelle
026900131008      **     Come Cliente prende solo la Ragione sociale e la data prima sped. fatturata
027000131008     C                   CLEAR                   BS69DS
027100131008     C                   CLEAR                   ACODS
027200131008     C                   MOVEL     KNSIF         I69SIF
027300131008     C                   MOVEL     inp_KSC       I69KAC
027400131008     C                   CALL      'TIBS69R'
027500131008     C                   PARM                    BS69DS
027600131008     C                   PARM                    ACODS
027700131008     C                   PARM                    INDDS
027800131008     C                   PARM                    CLPDS
027900131008     C                   PARM                    CLSDS
028000131008     c* Ragione Sociale Cliente
028100131008     C     O69ERR        IFne      '1'
028200131008     c                   movel     acorag        inp_CLD
028300131008      *  Memorizza la Data Bolla più vecchia  del CLIENTE ( non UNIFICANTE)
028400131008     c                   move      clpdps        inp_DPS
028500131008     c                   movel     clpdps        anno2             2 0
028600131008     c                   if        anno2 > 40
028700131008     c                   movel     '19'          inp_DPS
028800131008     c                   else
028900131008     c                   movel     '20'          inp_DPS
029000131008     C                   End
029100131008     c                   endIF
029200131008      * Poi
029300131008     c*  prende tutto dall'unificante
029400131007     C                   CLEAR                   BS10DS
029500131008     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
029600131008     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
029700131007     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
029800131007     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
029900131007     C                   MOVE      inp_KSC       D10COD
030000131007     C                   CALL      'TIBS10R'
030100131007     C                   PARM                    BS10DS
030200131008     C                   Z-ADD     inp_KSC       inp_KSU
030300131008IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
030400131008     C     D10COP        ANDNE     *ZEROS
030500131008     C                   Z-ADD     D10COP        inp_KSU                        *UNIFICANTE
030600131008E   2C                   ENDIF
030700131007      **
030800131007     C                   CLEAR                   BS69DS
030900131007     C                   CLEAR                   ACODS
031000131007     C                   MOVEL     KNSIF         I69SIF
031100131007     C                   MOVEL     inp_KSU       I69KAC
031200131008     C                   MOVEL     inp_KSU       I69KCP
031300131007     C                   CALL      'TIBS69R'
031400131007     C                   PARM                    BS69DS
031500131007     C                   PARM                    ACODS
031600131007     C                   PARM                    INDDS
031700131007     C                   PARM                    CLPDS
031800131007     C                   PARM                    CLSDS
031900131008     c* Descrizione Cliente Uificante
032000131008     C                   MOVEL     'C'           inp_CLV
032100131007     C     O69ERR        IFne      '1'
032200131008     c                   movel     clpclv        inp_CLV
032300131007     c                   movel     acorag        inp_DKS
032400131008      *
032500131008     C*  Settore Merceologico
032600131008     c                   move      acoitc        inp_SME
032700131008     C                   Z-ADD     1             F
032800131008     C     inp_SME       LOOKUP    SME(F)                                 99
032900131008IF  1C     *IN99         IFEQ      *ON
033000131008     C                   movel     DSME(F)       inp_DSM
033100131008     c                   end
033200131008      **
033300131007     c                   endIF
033400131008     C*
033500131008     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
033600131008     C                   Z-ADD     *ZEROS        inp_AGE                        *AGENTE A ZERO  =
033700131008IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
033800131008     C     clpAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
033900131008     C                   MOVEL     inp_KSU       NUM3              3 0          *CODICE FILIALE +
034000131008     C                   MOVEL     NUM3          inp_AGE                        *FIL+0000
034100131008X   1C                   ELSE                                                   *NO ERRORE
034200131008     C                   Z-ADD     clpAGE        inp_AGE                        *CODICE AGENTE
034300131008E   1C                   ENDIF
034400050705     C*--------------------
034500050705     c* Descrizione Agente Commerciale
034600050705     C*
034700050705     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
034800050706     C                   Z-ADD     1             C                 5 0
034900050705     C                   SETOFF                                           99
035000050705     C     inp_AGE       LOOKUP    AGE(C)                                 99
035100050705IF  1C     *IN99         ifeq      *ON
035200050705IF  2C     RGF(C)        IFEQ      *ZEROS
035300050705     C                   Z-ADD     inp_AGE       inp_RGF
035400050705     C                   MOVEL     *ALL'*'       inp_AGD                        DESCRIZIONE RAGGRUP
035500050705X   2C                   ELSE
035600050705     C                   Z-ADD     RGF(C)        inp_RGF
035700050705     C*
035800050705     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
035900050705     C                   Z-ADD     1             C
036000050705     C                   SETOFF                                           99
036100050705     C     inp_RGF       LOOKUP    AGE(C)                                 99
036200050705IF  3C     *IN99         IFEQ      *ON
036300050705     C                   MOVEL     DAGE(C)       inp_AGD                        DESCRIZIONE RAGGR.TO
036400050705X   3C                   ELSE
036500050705     C                   MOVEL     *ALL'*'       inp_AGD                        DESCRIZIONE RAGGR.TO
036600050705E   3C                   ENDIF
036700050705E   2C                   ENDIF
036800050705E   1C                   ENDIF
036900131008     C*--------------------
037000131008     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE dell'UNIFICANTE
037100131008     c                   movel     inp_KSU       inp_FIL
037200131008     C                   Z-ADD     1             F
037300131008     C     inp_FIL       LOOKUP    FIL(F)                                 99
037400131008IF  1C     *IN99         IFEQ      *ON
037500131008     C                   movel     dFIL(F)       inp_FID
037600131008     C                   Z-ADD     ARE(F)        inp_ARE
037700131008     C                   movel     dARE(F)       inp_ARD
037800131008     C                   MOVEL     DIV(F)        inp_DIV
037900131008     C                   movel     dDIV(F)       inp_DID
038000131008E   1C                   Else
038100131008     C                   clear                   inp_FID
038200131008     C                   clear                   inp_ARE
038300131008     C                   clear                   inp_ARD
038400131008     C                   clear                   inp_DIV
038500131008     C                   clear                   inp_DID
038600131008E   1C                   ENDIF
038700050705     C*--------------------
038800050705     c                   endsr
038900050705     ?****************************************************************************
039000050705      *?    Controlla se deve considerare il dettaglio in esame
039100050705     ?****************************************************************************
039200050705     c     Chk_Bolla     begsr
039300050705      *
039400050704     c                   eval      reck_OK = 'S'
039500050705      **
039600050705     ?** Controlla la Linea di Arrivo se è da considerare
039700050705     ?**  deve essere compresa fra le linee con il Network
039800050705     C                   Z-ADD     1             F
039900050705     C     tasLNA        LOOKUP    FILN(F)                                99
040000050705IF  1C     *IN99         ifeq      *Off
040100050705     c                   eval      reck_OK = 'N'
040200050705     c                   goto      end_check
040300050705     c                   else
040400050705      * Imposta il Network
040500050706     c                   move      NTW(F)        inp_NTW
040600050706      **
040700050706      * ? Fedex  --> Cambia il codice Network in 2 :
040800050706      * ?       Divide il Network "FED" in Buste e Merci --> "FEB" e "FEM"
040900050706     c                   if        inp_NTW  = ntwFED
041000050706     c                   z-add     tasCTR        campo2_0          2 0
041100050726     c                   if        campo2_0 >=50
041200050706     c                   move      'FEB'         inp_NTW
041300050706     c                   end
041400051014     c                   if        campo2_0 <  50
041500050706     c                   move      'FEM'         inp_NTW
041600050706     c                   end
041700050706     c                   end
041800050706     c
041900050705     c                   end
042000050704      **
042100050718     ?** Controlla come devono essere totalizzate le bolle x qtà o valore
042200050705     C                   Z-ADD     1             F
042300050705     C     tasTBL        LOOKUP    TBL(F)                                 99
042400050705IF  1C     *IN99         ifeq      *ON
042500050705      * Imposta tipo Porto
042600050705     c                   eval      inp_FA  = TPO(F)
042700050705     c                   eval      inp_RBL = RBL(F)
042800050718     c                   end
042900050718      *
043000050718     ?** Ma Se ha Note di Credito (STORNO Totale) NON deve considerare
043100050718     ?**     il valore di tassazione ma solo le quantità.
043200050718     c                   clear                   NotaCredito       1
043300050718     c                   if        tasFA2  = 'S'
043400050718     c                   move      'S'           NotaCredito
043500050718     c                   end
043600050517      **
043700050704     c     end_check     endsr
043800050705     ?****************************************************************************
043900050705      *?    Controlla se deve considerare il dettaglio in esame
044000050705     ?****************************************************************************
044100050705     c     Decod_Bolla   begsr
044200050705      *
044300050705      * ?Controlla la Tariffa se appartiene al Network
044400050705     c                   clear                   inp_NOT
044500050705     C     keyTAM        chain     TNTAM01L
044600050706     c                   if        not %Found(TNTAM01L)
044700050706     c                   move      'N'           inp_NOT
044800050706     c                   else
044900050705     c                   move      tamFLO        dsTA01
045000050705      * ? DPD
045100050705     c                   if        tamFIE ='I' and §taDPD = 'D' and
045200050705     c                             inp_NTW <> 'DPD'
045300050705     c                   move      'N'           inp_NOT
045400050705     c                   end
045500050705      * ? Fedex
045600050705     c                   if        tamFIE ='E' and §taFED = 'F' and
045700050705     c                             inp_NTW <> 'FED'
045800050705     c                   move      'N'           inp_NOT
045900050706     c                   end
046000050705      * ? EEX
046100050705     c                   if        tamFIE ='E' and §taFED = ' ' and
046200050705     c                             inp_NTW <> 'EEX'
046300050705     c                   move      'N'           inp_NOT
046400050705     c                   end
046500050705      **
046600050705     c                   end
046700050705      *
046800050705      * ? Nazione Destinatario
046900050705     c                   eval      inp_NAZ = tasNZD
047000050718     c                   clear                   inp_DNA
047100050718     C                   Z-ADD     1             G
047200050718     C     inp_NAZ       LOOKUP    NAZ(G)                                 99
047300050718IF  1C     *IN99         ifeq      *ON
047400050718     c                   movel     DNAZ(G)       inp_DNA
047500050718     c                   end
047600050707      *
047700050707      * ? Data Spedizione
047800050707     c                   move      tasANNOme     inp_DIN
047900050707     c                   move      tasANNOme     inp_DFI
048000050705      **
048100050705     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME SPEDIZIONE o COME RICAVO:
048200050705     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
048300050705     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
048400050705     C* CON FLAG FRI='S';
048500050705     C                   Z-ADD     1             G
048600050705     C                   SETOFF                                           99
048700050705     C     inp_RBL       LOOKUP    FLAB(G)                                99
048800050705IF  1C     *IN99         ifeq      *ON
048900050705     C*
049000050705      * ? Bolla VALIDA come Spedizione
049100050705IF  1C     FSP(G)        IFEQ      'S'
049200050705     c                   z-add     tasNCL        Inp_TCOLLI
049300050705     c                   z-add     1             Inp_TSPED
049400050705     c                   z-add     tasVLF        Inp_TVOL
049500050705     c                   z-add     tasPKF        Inp_TPES
049600050705E   1C                   ENDIF
049700050705     C*
049800050708      * ? Bolla VALIDA come RICAVO
049900050705IF  1C     FRI(G)        IFeq      'S'
050000050718IF  1C     NotaCredito   andeq     *blank                                       <- Non Stornata totalmente
050100050705     c                   z-add     tasPOR        Inp_TFAT
050200050705     c                   add       tasVA1        Inp_TFAT
050300050705     c                   add       tasVA2        Inp_TFAT
050400050705     c                   add       tasVA3        Inp_TFAT
050500050705     c     ktas3         setll     tita730c
050600050705     c     ktas3         reade     tita730c
050700050705     c                   dow       not %Eof(tita730c)
050800050705     c                   add       ta7VAn        Inp_TFAT
050900050705     c     ktas3         reade     tita730c
051000050705     c                   enddo
051100050708     C                   endIF
051200050705      *
051300050705      * ? IMPORTO Competenze ossia  RICAVO Presunto --> per calcolo Delta
051400050705     c                   clear                   inp_TCMP
051500050711      *
051600050711      *  se più vecchie di 10 mesi non sono più presenti su ECCEV
051700050711      *    quindi occorre andare su ECCET e ECCED
051800050711      *
051900050711     c     ktas3         chain     Eccet30C
052000050711     c                   iF        %Found(Eccet30C)
052100050711     c                   add       cetPAR        inp_TCMP
052200050711     c                   add       cetPAM        inp_TCMP
052300050711     c                   add       cetPAU        inp_TCMP
052400050711     c                   add       cetPAV        inp_TCMP
052500050711     c                   add       cetARD        inp_TCMP
052600050711     c                   add       cetARM        inp_TCMP
052700050711     c                   add       cetARU        inp_TCMP
052800050711     c                   add       cetARV        inp_TCMP
052900050711     c                   add       cetTRL        inp_TCMP
053000050711     c                   add       cetPAG        inp_TCMP
053100050711     c                   add       cetSEA        inp_TCMP
053200050711     c                   add       cetSEC        inp_TCMP
053300050711     c                   add       cetRCV        inp_TCMP
053400050711     c                   add       cetMDE        inp_TCMP
053500050711     c                   add       cetTDE        inp_TCMP
053600050711     c                   add       cetMAF        inp_TCMP
053700050711     c                   add       cetTAF        inp_TCMP
053800050711     c                   add       cetSES        inp_TCMP
053900050711      *
054000050711E   1C                   End
054100050711      *
054200050711     c     ktas3         setll     Ecced30C
054300050711     c     ktas3         reade     Ecced30C
054400050711     c                   DoW       not %Eof(Ecced30C)
054500050712      ** deve escludere i transiti
054600050712     c                   eval      transiti = 'N'
054700050712     c                   exsr      Check_Trans
054800050712     c                   if        transiti = 'N'
054900050711     c                   add       cedIMP        inp_TCMP
055000050712     c                   end
055100050711     c     ktas3         reade     Ecced30C
055200050711E   1C                   End
055300050711      *
055400050711E   1C                   EndiF
055500050705     C*
055600050705     c                   endsr
055700050705     ?****************************************************************************
055800050705      *?    Scrive Riga File di Work
055900050705     ?****************************************************************************
056000050712     c     Check_Trans   begsr
056100050705      *
056200050712      *  Competenze di transito
056300050712     c                   if        CEDCMP = 11   or
056400050712     c                             CEDCMP = 12   or
056500050712     c                             CEDCMP = 53   or
056600050712     c                             CEDCMP = 54   or
056700050712     c                             CEDCMP = 55   or
056800050712     c                             CEDCMP = 204  or
056900050712     c                             CEDCMP = 205  or
057000050712     c                             CEDCMP = 206  or
057100050712     c                             CEDCMP = 207  or
057200050712     c                             CEDCMP = 208
057300050712     c                   move      'S'           transiti          1
057400050712     c                   end
057500050712     C*
057600050712     c                   endsr
057700050712     ?****************************************************************************
057800050712      *?    Scrive Riga File di Work
057900050712     ?****************************************************************************
058000050712     c     Wri_rig_det   begsr
058100050712      *
058200050705      * ? x Chiave completa
058300050712     c     Key_CEX       chain     wfcex01l
058400050705      * ? aggiorna
058500050712     c                   iF        %Found(wfcex01L)
058600050707      * ?  data Ultima
058700050707     c                   If        inp_DFI > cexDFI
058800050707     c                   move      inp_DFI       cexDFI
058900050707     c                   eNd
059000050705      * ? Aggiunge ai Valori di Periodo
059100050705     c                   exsr      add_Valori
059200050712     c                   update    WFCEX000
059300050513      *
059400050513     c                   eLSe
059500050705      * ? scrive
059600050712     c                   clear                   WFCEX000
059700050708     c                   move      inp_ANN       CEXANN
059800050708     c                   move      inp_MES       CEXMES
059900050708     c                   move      inp_DIV       CEXDIV
060000050705     c                   move      inp_DID       CEXDID
060100050705     c                   move      inp_ARE       CEXARE
060200050705     c                   move      inp_ARD       CEXARD
060300050705     c                   move      inp_FIL       CEXFIL
060400050705     c                   move      inp_FID       CEXFID
060500050705     c                   move      inp_AGE       CEXAGE
060600050705     c                   move      inp_AGD       CEXAGD
060700050705     c                   move      inp_KSC       CEXKSC
060800050705     c                   move      inp_CLD       CEXCLD
060900050705     c                   move      inp_CLV       CEXCLV
061000050708     c                   movel     inp_DPS       CEXDAT
061100050706     c                   move      inp_SME       CEXSME
061200050718     c                   move      inp_DSM       CEXDSM
061300130927      *- se non è unificato che a se stesso imposta lo stesso codice
061400130927     c                   if        inp_KSU = 0
061500130927     c                   eval      inp_KSU = inp_KSC
061600130927     c                   end
061700050708     c                   move      inp_KSU       CEXKSU
061800050705     c                   move      inp_DKS       CEXDKS
061900050705     c                   move      inp_NTW       CEXNTW
062000050705     c                   move      inp_NOT       CEXNOT
062100050705     c                   move      inp_NAZ       CEXNAZ
062200050718     c                   move      inp_DNA       CEXDNA
062300050705     c                   move      inp_FA        CEXFA
062400050707      **
062500050707      * ?  data Prima Spedizione
062600050707     c                   z-add     inp_DIN       CEXDIN
062700050707     c                   z-add     inp_DFI       CEXDFI
062800050705      **
062900050705      * ? Imposta i Valori di Periodo
063000050707     c                   move      AnnoINI       CEXDAN
063100050707     c                   move      MeseINI       CEXDME
063200050707     c                   move      se83ANN       CEXAAN
063300050707     c                   move      se83MES       CEXAME
063400050705     c                   exsr      add_Valori
063500050712     c                   write     WFCEX000
063600050705      *
063700050513     c                   eNd
063800050517      *
063900050513     c                   endsr
064000050704     ?****************************************************************************
064100050705      *?   Aggiunge i valori di totalizzazione
064200050704     ?****************************************************************************
064300050705     C     add_valori    BEGSR
064400050704      **
064500050705     c                   add       INP_TSPED     cexTSPED
064600050705     c                   add       INP_TCOLLI    cexTCOLLI
064700050705     c                   add       INP_TVOL      cexTVOL
064800050705     c                   add       INP_TPES      cexTPES
064900050705     c                   add       INP_TFAT      cexTFAT
065000090923     c                   z-add(h)  INP_TCMP      TCMPs
065100090923     c                   add       tcmps         cexTCMP
065200050705      *
065300050705     c                   endsr
065400050705     ?****************************************************************************
065500050705      *?   Totali x Cliente
065600050705     ?****************************************************************************
065700050705     C     Tot_Cliente   BEGSR
065800050705      *
065900050705     c                   endsr
066000050705     ?****************************************************************************
066100050705      *?   INIZIALIZZAZIONE
066200050705     ?****************************************************************************
066300050705     C     *inzsr        BEGSR
066400050705      **
066500040820     C     *ENTRY        PLIST
066600040820     C                   PARM                    KPJBA
066700050513     c                   movel     kpjbu         DsPassaggio
066800050705      ** Pulizia schiere
066900050705     c                   clear                   FIL
067000050705     c                   clear                   DFIL
067100050705     c                   clear                   FILN
067200050705     c                   clear                   NTW
067300050705     c                   clear                   ARE
067400050705     c                   clear                   DARE
067500050705     c                   clear                   DIV
067600050705     c                   clear                   DDIV
067700050705     c                   clear                   FLAB
067800050705     c                   clear                   FSP
067900050705     c                   clear                   FRI
068000050705     c                   clear                   TBL
068100050705     c                   clear                   RBL
068200050705     c                   clear                   TPO
068300050705     c                   clear                   AGE
068400050705     c                   clear                   RGF
068500050705     c                   clear                   DAGE
068600050511      **
068700050511     C                   Z-ADD     1             CODUT
068800050511     C                   CALL      'X§PARUT'
068900050511     C                   PARM                    UT§DSE
069000110511     C*******            MOVEL     RAGUT         RSUT             20
069100110511     C                   MOVEL     REC42         CNCR42
069200110511     C                   MOVEL     simRGS        RSUT             20
069300041103      *
069400040820      *  imposta UDATE di elaborazione
069500040820     C                   move      *date         G02DAT
069600040820     C                   MOVE      *ZEROS        G02INV
069700040820     C                   MOVE      *BLANKS       G02ERR
069800040820     C                   CALL      'XSRDA8'
069900040820     C                   PARM                    WLBDAT
070000040820     C                   Z-ADD     G02INV        data_oggi
070100050513     C                   Z-ADD     G02INV        DATCOR            8 0          *DATA (8) IN AA/M/G
070200050513     C* IMNPOSTA DATA / ORA CORRENTE
070300050712     C                   move      G02dat        day               8 0
070400050513     C                   TIME                    W0140            14 0          *ORA (6)+ DATA (8)
070500050712     C                   MOVEl     W0140         ore               6 0
070600050706      **
070700050706     c                   if        se83Dan = 0 or se83dMe = 0
070800050704      **  anno precedente
070900050704     c     se83ANN       sub       1             annoINI           4 0
071000050704      **  e 12 mesi prima
071100050704     c                   if        se83MES < 12
071200050704     c     se83MES       add       1             meseINI           2 0
071300050704     c                   else
071400050704      **  oppure gennaio dell'anno richiesto
071500050704     c                   z-add     se83ANN       annoINI
071600050704     c                   z-add     1             meseINI
071700050704     c                   end
071800050704      **
071900050706     c                   else
072000050706     c                   z-add     se83Dan       annoINI
072100050706     c                   z-add     se83DMe       meseINI
072200050706     c                   end
072300050707      **
072400050704     c                   movel     annoINI       DataINI           6 0
072500050704     c                   move      meseINI       DataINI
072600050706     c                   movel     meseINI       MESEggINI         4 0
072700050704     c                   move      '01'          MESEggINI
072800050707      **  anno finale
072900050704     c                   movel     se83ANN       DataFIN           6 0
073000050704     c                   move      se83MES       DataFIN
073100050707     c                   z-add     se83ANN       annoFIN
073200050706     c                   movel     se83MES       MESEggFIN
073300050704     c                   move      '31'          MESEggFIN
073400050707      **
073500050707     c                   z-add     se83DAN       inp_DAN
073600050707     c                   z-add     se83DME       inp_DME
073700050708     c                   z-add     se83ANN       inp_AAN
073800050708     c                   z-add     se83MES       inp_AME
073900050704      **
074000050712     c                   move      DataINI       PDadata           6 0
074100050712     c                   move      DataFIN       Padata            6 0
074200050704      **
074300131008     C* COMPONE IL GIORNO finale DELLA DATA FINALE
074400131008     C                   MOVEL     Padata        WDFIN             8 0          *DATA INIZIALE
074500131008     C                   MOVE      28            WDFIN
074600131008     C*
074700131008     C*------- GIORNO   di   F I N E   M E S E ---------------------*
074800131008     C*
074900131008     C*  Ultimo giorno del mese
075000131008     C                   MOVE      WDFIN         MMGG_4            4 0
075100131008     C                   MOVEL     MMGG_4        Mm_2              2 0
075200131008     C                   MOVEl     WDFIN         Anno_4            4 0
075300131008      * se è febbraio
075400131008     c                   If        Mm_2 = 2
075500131008      *
075600131008      *  potrebbe essere bisestile 29 gg.
075700131008     c     Anno_4        div       4             risulta           3 0
075800131008     c                   mvr                     resto             3 0
075900131008      *  Se bisestile
076000131008     c                   If        resto = 0
076100131008     C                   MOVE      '29'          WDFIN
076200131008     c                   end
076300131008      *
076400131008     c                   else
076500131008      *
076600131008      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
076700131008     c                   If           Mm_2 = 4 or Mm_2 = 6 or
076800131008     c                                Mm_2 = 9 or Mm_2 = 11
076900131008     C                   MOVE      '30'          WDFIN
077000131008     c                   else
077100131008     C                   MOVE      '31'          WDFIN
077200131008     c                   end
077300131008      *
077400131008     c                   endIf
077500131008     C*
077600131008     C*----------- F I N E    M E S E ---------------------------*
077700131008     C*
077800050704     c                   clear                   ntwDPD
077900050704     c                   clear                   ntwEEX
078000050704     c                   clear                   ntwFED
078100050704      **
078200050704     c                   if        %subst(se83NTW:1:1) ='S'                     <- DPD
078300050704     c                   eval      ntwDPD ='DPD'
078400050704     c                   end
078500050704     c                   if        %subst(se83NTW:2:1) ='S'                     <- EEX
078600050704     c                   eval      ntwEEX ='EEX'
078700050704     c                   end
078800050704     c                   if        %subst(se83NTW:3:1) ='S'                     <- FED
078900050704     c                   eval      ntwFED ='FED'
079000050704     c                   end
079100040823      **
079200050511     c                   seton                                        66
079300040825     c                   clear                   prima_volta       1
079400040826     c                   clear                   almeno_uno
079500050712     C                   MOVEL     DSPGM         NOMPGM           10
079600050705     C                   Z-ADD     1             tblKUT
079700050513     C*--------------------
079800050704     C* ORGANIGRAMMA Carico solo filiali x LNA da selezionare in lettura bolle
079900050704     C*              appartenenti ai Network in esame.
080000050513     C*--------------------
080100050513     C                   Z-ADD     *ZEROS        F                 3 0
080200050513     C                   Z-ADD     *ZEROS        korFIL            3 0
080300050513     C     keyORG        SETLL     AZorg01L
080400050513     C                   READ      AZorg01L                               99
080500050513DO  1C     *IN99         DOWEQ     '0'
080600050513IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
080700050513IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
080800050704     c                   move      orgde3        og143
080900050704
081000050513     C                   ADD       1             F
081100050513     C                   Z-ADD     orgFIL        FIL(F)
081200050513     C                   MOVEL     orgDES        DFIL(F)
081300050704     c                   MOVEL     *blank        NTW(F)
081400050704      **
081500050704     c                   if        §OGNTW <> *Blank
081600050704     c                   if        §OGNTW = ntwDPD or
081700050704     c                             §OGNTW = ntwEEX or
081800050704     c                             §OGNTW = ntwFED
081900050704     C                   Z-ADD     orgFIL        FILN(F)
082000050704     c                   MOVEL     §OGNTW        NTW(F)
082100050704E   2C                   endIF
082200050704E   2C                   endIF
082300050704      **
082400050513     C                   MOVEL     '05'          tblCOD
082500050513     C                   MOVEL     *BLANKS       tblKEY
082600050513     C                   MOVEL     orgCAR        tblKEY
082700050513     C     keyTAB        CHAIN     TABEL00F                           99
082800050513IF  4C     *IN99         IFEQ      *OFF
082900050513     C                   Z-ADD     orgCAR        ARE(F)
083000050513     C                   MOVEL     TBLUNI        DARE(F)
083100050513E   4C                   ENDIF
083200050704      **
083300050513     C                   MOVEL     '17'          tblCOD
083400050513     C                   MOVEL     *BLANKS       tblKEY
083500050513     C                   MOVEL     orgFL3        tblKEY
083600050513     C     KEYTAB        CHAIN     TABEL00F                           99
083700050513IF  4C     *IN99         IFEQ      *OFF
083800050513     C                   MOVEL     orgFL3        DIV(F)
083900050513     C                   MOVEL     TBLUNI        DDIV(F)
084000050513E   4C                   ENDIF
084100050704
084200050513E   3C                   ENDIF
084300050513E   2C                   ENDIF
084400050704
084500050513     C                   READ      AZorg01L                               99
084600050513E   1C                   ENDDO
084700050517     C*-------------------------
084800050517     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
084900050517     C*-------------------------
085000050517     C                   MOVEL     '2W'          tblCOD
085100050517     C     KEYTA2        SETLL     TABEL00F
085200050705     C     KEYTA2        READE     TABEL00F
085300050517     C*
085400050517     C                   Z-ADD     *ZEROS        G                 4 0
085500050705DO  1C                   DOW       not %Eof(TABEL00F)
085600050517IF  2C     TBLFLG        IFEQ      *BLANKS
085700050517     C                   ADD       1             G
085800050517     C                   MOVEL     TBLKEY        FLAB(G)
085900050517     C                   MOVEL     TBLUNI        DS2W
086000050517     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
086100050517     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
086200050517E   2C                   ENDIF
086300050517     C*
086400050517     C     KEYTA2        READE     TABEL00F                               99
086500050517E   1C                   ENDDO
086600050718     C*-------------------------
086700050718     C* DECODIFICHE SETTORE MERCEOLOGICO
086800050718     C*-------------------------
086900050718     C                   MOVEL     '1L'          tblCOD
087000050718     C     KEYTA2        SETLL     TABEL00F
087100050718     C     KEYTA2        READE     TABEL00F
087200050718     C*
087300050718     C                   Z-ADD     *ZEROS        G                 4 0
087400050718DO  1C                   DOW       not %Eof(TABEL00F)
087500050718IF  2C     TBLFLG        IFEQ      *BLANKS
087600050718     C                   ADD       1             G
087700050718     C                   MOVEL     TBLKEY        SME(G)
087800050718     C                   MOVEL     TBLUNI        DS1L
087900050718     C                   MOVEL     §1LDES        DSME(G)                        *DESCRIZIONE
088000050718E   2C                   ENDIF
088100050718     C*
088200050718     C     KEYTA2        READE     TABEL00F                               99
088300050718E   1C                   ENDDO
088400050718     C*-------------------------
088500050718     C* DECODIFICHE NAZIONI
088600050718     C*-------------------------
088700050718     C                   MOVEL     '15'          tblCOD
088800050718     C     KEYTA2        SETLL     TABEL00F
088900050718     C     KEYTA2        READE     TABEL00F
089000050718     C*
089100050718     C                   Z-ADD     *ZEROS        G                 4 0
089200050718DO  1C                   DOW       not %Eof(TABEL00F)
089300050718IF  2C     TBLFLG        IFEQ      *BLANKS
089400050718     C                   ADD       1             G
089500050718     C                   MOVEL     TBLKEY        NAZ(G)
089600050718     C                   MOVEL     TBLUNI        DS15
089700050718     C                   MOVEL     §15DES        DNAZ(G)
089800050718E   2C                   ENDIF
089900050718     C*
090000050718     C     KEYTA2        READE     TABEL00F                               99
090100050718E   1C                   ENDDO
090200050705     C*--------------------
090300050705     C* TIPI BOLLA
090400050705     C*--------------------
090500050705     C                   MOVEL     'TB'          tblCOD
090600050705     C                   Z-ADD     *ZEROS        G
090700050705     C     keyTA2        setll     TABEL00F
090800050705     C     keyTA2        reade     TABEL00F
090900050705DO  1C                   DOW       not %Eof(TABEL00F)
091000050705IF  2C     tblFLG        IFEQ      *BLANKS
091100050705     C                   ADD       1             G
091200050705     C                   MOVEL     tblKEY        TBL(G)
091300050705     C                   MOVEL     tblUNI        DSTB
091400050705     C                   MOVEL     §TBRBL        RBL(G)
091500050705     C                   MOVEL     §TBTPO        TPO(G)
091600050705E   2C                   ENDIF
091700050705     C     keyTA2        reade     TABEL00F
091800050705E   1C                   ENDDO
091900050705     C*--------------------
092000050705     C* COMMERCIALI
092100050705     C*--------------------
092200140612     C*********          MOVEL     '01'          tblCOD
092300140612     C*********          Z-ADD     *ZEROS        G
092400140612     C***  KEYTA2        SETLL     TABEL00F
092500140612     C***  KEYTA2        READE     TABEL00F
092600140612DO  1C*********          DOW       not %Eof(TABEL00F)
092700140612IF  2C***  TBLFLG        IFEQ      *BLANKS
092800140612     C*********          ADD       1             G
092900140612     C*********          MOVEL     TBLUNI        DS01
093000140612     C*********          MOVEL     TBLKEY        AGE(G)
093100140612     C*********          MOVEL     §01RGF        RGF(G)
093200140612     C*********          MOVEL     §01AGE        DAGE(G)
093300140612E   2C*********          ENDIF
093400140612     C***  KEYTA2        READE     TABEL00F
093500140612E   1C*********          ENDDO
093600140612      *********
093700140612     C                   Z-ADD     *ZEROS        G
093800140612     c                   open      azCMM01l
093900140612     C     *loval        SETLL     azCMM01l
094000140612     C                   READ      azCMM01l
094100140612DO  1C                   DOW          not %Eof(azCMM01l)
094200140612IF  2C                   IF           cmmATB = *blank
094300140612     C                   ADD       1             G
094400140612     C                   z-add     cmmCOD        AGE(G)
094500140612     C                   z-add     cmmUNI        RGF(G)
094600140612     C                   MOVEL     cmmDES        DAGE(G)
094700140612E   2C                   ENDIF
094800140612     C                   READ      azCMM01l
094900140612E   1C                   ENDDO
095000140612     c                   close     azCMM01l
095100050705     C*--------------------
095200050513     C* CHIAVE LETTURA AZORG01L - COMPLETA
095300050513     C     keyORG        KLIST
095400050513     C                   KFLD                    korFIL                         *FILIALE
095500050513     C*
095600050513     C* CHIAVE LETTURA TABEL00F - COMPLETA
095700050513     C     keyTAB        KLIST
095800050513     C                   KFLD                    tblKUT                         *CODICE UTENTE
095900050513     C                   KFLD                    tblCOD                         *CODICE TABELLA
096000050513     C                   KFLD                    tblKEY                         *CHIAVE TABELLA
096100050517     C*
096200050517     C* CHIAVE LETTURA TABEL00F - parziale
096300050517     C     keyTA2        KLIST
096400050517     C                   KFLD                    tblKUT                         *CODICE UTENTE
096500050517     C                   KFLD                    tblCOD                         *CODICE TABELLA
096600050704     C*
096700050704     C* CHIAVE LETTURA TITAS38C - parziale
096800050705     C     TAS_INI       KLIST
096900050704     C                   KFLD                    AnnoINI
097000050704     C                   KFLD                    MeseggINI
097100050513     C*
097200050705     C* CHIAVE LETTURA TNTAM01L
097300050705     C     keyTAM        KLIST
097400050705     C                   KFLD                    tasKSC
097500050705     C                   KFLD                    tasCTR
097600050705      *
097700050705     c     ktas3         klist
097800050705     c                   kfld                    tasAas
097900050705     c                   kfld                    tasLnp
098000050705     c                   kfld                    tasNrs
098100050705     c                   kfld                    tasNsp
098200050705     c                   kfld                    tasTbl
098300050705      **
098400050705     c     key_cex       klist
098500050708     c                   kfld                    inp_ANN
098600050708     c                   kfld                    inp_MES
098700050708     c                   kfld                    inp_KSC
098800050705     c                   kfld                    inp_NTW
098900050705     c                   kfld                    inp_NAZ
099000050705     c                   kfld                    inp_FA
099100050708     c                   kfld                    inp_NOT
099200050705      *
099300050513     C                   ENDSR
099400050705     ?****************************************************************************
099500050705      *?   Totali da  File di Work
099600050705     ?****************************************************************************
099700050705     c     Tot_Fine      begsr
099800050705      * ?--
099900050705      * ?--
100000050705     c                   endsr
100100050705     ?****************************************************************************
