000100120723     /*PRM dbgview(*source)
000200120723     /*CMD ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120723     /*CMD        ovrscope(*calllvl)
000400160913     /*CMD ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
000500160913     /*CMD        ovrscope(*calllvl)
000600160913     /*END dltovr file(TITAS30C FNARB01L) lvl(*)
000700120723     /*END
000800981007      *-------------------------------------------------------------------------*
000900981014      *               CHIUSURA C.A./RIAPERTURA C.A.                             *
001000981007      *-------------------------------------------------------------------------*
001100981016      * Questo pgm potrebbe essere richiamato da altri
001200981016      *
001300981016      * Il controllo del chisono :partenza /arrivo/sede lo faccio con
001400981016      * il P.O. gestione perchè questa fase è eseguita solo da chi ha
001500981016      * in gestione la C.A.
001600050331      *
001700050331      * Inserito il controllo in fase di riapertura dell'utente se autorizzato
001800050331      * per le CA chiuse con causale per le quali si deve fare controllo
001900050331      * pilotato dalla tabella CCH
002000981014      ****************************************************************
002100981014      *  FASI C.A. FISSATE A PROGRAMMA
002200981014      ****************************************************************
002300981014      *    900 = Chiusura CA         (WCHCA)
002400981105      *    250 = Rifiuto            liquidazione transattiva   (WRIFI)
002500981105      *    230 = Contabilizzazione  liquidazione transattiva   (WCOLT)
002600090925      *    215 = Esito (OBSOLETO)   liquidazione transattiva   (WESIT)
002700090925      *    190 = Avviso danno LT    liquidazione transattiva   (WAVDT)
002800990430      *    225 = Ritorno al pagamento liquidazione transattiva (WRITI)
002900050126      *    700 = Progetto liquidazione                         (WPRLI)
003000050331      *    690 = Ristampa Progetto liquidazione                (WRIPL)
003100120507      *    545 = Avviso Danno                                  (WAVVD)
003200990311      ****************************************************************
003300990311      *  CAUSALI FISSATE A PROGRAMMA
003400990311      ****************************************************************
003500120507      *    06  = Gestione Uff.Internazionale      (WCHEE)
003600990311      *    07  = Pagamento pratica assicurativa   (WCHPA)
003700020226      *    15  = Pagamento pratica in franchigia  (WCHFR)
003800050126      *    17  = Pagamento pratica al posto di MOSCA (WCHBA)
003900131011      *    18  = Pagamento pratica al posto di A.I.G.(WCHBB)
004000080115      *    19  = Bonifico manuale                    (WCHBM)
004100981014      ***************************************************************
004200981013      *
004300980521      ****************************************************************
004400980521      *  RIEPILOGO INDICATORI
004500980521      ***************************************************************
004600980521      * 05 - SPROTEGGO P.O. IN GESTIONE
004700980904      * 06 - NON ABILITO TASTO FUNZIONALE F18
004800981001      * 07 - NON VISUALIZZO "P.O. IN GESTIONE" PER SEDE
004900981014      * 08 - RIPRISTINO C.A. richiesta fase CA
005000981030      * 10 - Proteggo il cmd3 quando il programma è richiamato
005100981105      * 20 - Riapertura CA per rifiuto assegno di traenza
005200981105      * 21 - Posizionamento cursore
005300050331      * 22 - Riapertura CA per rifiuto assegno di traenza pratica in surroga
005400160913      * 23 - Chiusura C.A. con causale 18 = Pagamento Pratica BRT
005500160913      * 24 - Chiusura C.A. prescritta per decorsi termini
005600980923      * 28 - ERRORE GENERICO DSPF
005700980904      * 30 - COMODO
005800980904      * 31 - COMODO
005900981016      * 32 - COMODO
006000160914      * 33 - COMODO
006100980904      * 40 - ERRORE p.o. in gestione
006200981013      * 41 - ERRORE numero C.A.
006300981013      * 42 - ERRORE causale di chiusura
006400160921      * 71 - Chiusura C.A. con Pratica affidata ad A.I.G.
006500160921      * 72 - Chiusura C.A. con Pratica Assicurativa
006600160921      * 73 - Chiusura C.A. con Liquidazione Transattiva
006700160921      * 74 - Chiusura C.A. con P.A. senza Progetto Liquidazione
006800160921      * 75 - Chiusura C.A. già estratta per il pagamento
006900160921      * 76 - Chiusura C.A. già in pagamento
007000160913      * 90 - Riemissione videata
007100980904      ***************************************************************
007200980623
007300980521     H DECEDIT('0,') DATEDIT(*DMY.)
007400980623
007500990122     fFIDN22D   CF   E             WORKSTN  usropn
007600981013     fFNDCT01L  UF   E           K DISK
007700051020     fFNDET01L  Uf   e           k disk
007800981014     fFNDCD01L  UF   E           K DISK
007900981001     fFNDCF01L  UF A E           K DISK
008000080115     fFNDCL01L  UF A E           K DISK     usropn
008100050331     fFNDCS01L  UF A E           K DISK
008200040722     fFNDCK01l  IF   E           K DISK
008300050922     fFNDRA01l  IF   E           K DISK
008400050404     fTigcp21L  IF   E           K DISK     usropn
008500981016     fFNarb01L  UF   E           K DISK     usropn
008600981001     fAZORG01L  IF   E           K DISK
008700010926     fTITAS30C  IF   E           K DISK     usropn
008800090417     fFnRIG00F  O    E             DISK     usropn
008900050218     fTabel00f  if   e           k disk
009000150702     fFNDKA01L  uf   e           k disk
009100980623
009200980521      *------------------------------------------------------------------------*
009300981014     D Wfil            S                   LIKE(DCTfil)
009400981013     D Kaac            S                   LIKE(DCTaac)
009500981013     D Kfil            S                   LIKE(DCTfil)
009600981013     D Knca            S                   LIKE(DCTnca)
009700040722
009800040722     D Giaaas          S                   LIKE(DCTaas)
009900040722     D Gialnp          S                   LIKE(DCTlnp)
010000040722     D Gianrs          S                   LIKE(DCTnrs)
010100040722     D Giansp          S                   LIKE(DCTnsp)
010200040722
010300090925     d Wavdt           s                   like(dctfca)
010400090925     d                                     inz(190)
010500090925     d****  Wesit           s                   like(dctfca)
010600090925     d****         OBSOLETO                    inz(215)
010700981014     d Wchca           s                   like(dctfca)
010800981014     d                                     inz(900)
010900981105     d Wcolt           s                   like(dctfca)
011000981105     d                                     inz(230)
011100981105     d Wrifi           s                   like(dctfca)
011200981105     d                                     inz(250)
011300990430     d Writi           s                   like(dctfca)
011400990430     d                                     inz(225)
011500050126     d Wprli           s                   like(dctfca)
011600050126     d                                     inz(700)
011700050401     d Wripl           s                   like(dctfca)
011800050331     d                                     inz(690)
011900120507     d Wavvd           s                   like(dctfca)
012000120507     d                                     inz(545)
012100981130     d Wfaca           s                   like(dctfca)
012200981016     D wfrg            s                   LIKE(GCPfrg)
012300120507     d wCHEE           s                   like(dctcch)
012400120507     d                                     inz('06')
012500990311     d Wchpa           s                   like(dctcch)
012600990311     d                                     inz('07')
012700020226     d WchFR           s                   like(DCTcch)
012800020226     d                                     inz('15')
012900050126     d WchBA           s                   like(DCTcch)
013000050126     d                                     inz('17')
013100050511     d WchBB           s                   like(DCTcch)
013200050511     d                                     inz('18')
013300080115     d WchBM           s                   like(DCTcch)
013400080115     d                                     inz('19')
013500980923
013600080123     d wSAVcch         s                   like(DCTcch) inz
013700980923     D WORA            S              4  0
013800980721     D W0140           S             14  0
013900980521     D Wdtgio          S              8  0
014000980521     D dateu           S              8  0
014100990712     D wrkdta          S              8  0
014200981015     D wdataca         S              8  0
014300981230     D w§dctlnpc       S              3  0
014400981016     d chisono         s              1                                         * A=ARR P=PART
014500990311     D wnco            S              5  0
014600990712     D wdata           S               D   DATFMT(*iso)
014700010926     D $over4m         s              1    inz(*off)
014800020128     D $Franch         s              1    inz(*off)
014900020128     D w03an           s              3    inz(*zeros)
015000020128     D w04an           s              4    inz(*zeros)
015100020129     D w07an           s              7    inz(*zeros)
015200070404     d wfilgeo         s              1    inz(*off)
015300080114     d $Video          s              2    inz
015400080114     d $InzD2          s               n   inz(*on)
015500120507     d Wdaspe          s              8  0
015600120507
015700120507     d wPFIEEX         s               n   inz(*off)                            Riapro PF IMPORT EEX
015800980923
015900050511      *  Data LIMITE per causale chiusura 17 -> 18
016000050511     d C_DataChiusLim  c                   const(20050417)
016100980622      *  titolo videata (lunghezza massima 34)
016200080114     D TIT_chca        C                   CONST('          CHIUSURA C.A.      -
016300981013     D                                          ')
016400981014     D TIT_raca        C                   CONST('         RIAPERTURA C.A.     -
016500981014     D                                          ')
016600020129      *
016700980521      *   S C H I E R E
016800080115     D MSG             S             78    DIM(22) CTDATA PERRCD(1)              MSG VIDEO
016900981014     D L6              S              3  0 DIM(30)                              P.O.GESTITI
017000981016     D L1              S              3  0 DIM(30)                              P.O.GESTITI
017100980923      *
017200980923     D WLBDAT          DS                  INZ
017300980923     D  G02DAT                 1      8  0
017400980923     D  G02INV                 9     16  0
017500980923     D  G02ERR                17     17
017600980923     D  G02TGI                18     22  0
017700980915      *----------------------------------
017800050929      * - Parametri x Controllo profilo utenti
017900050929     d TIBS34DS      e ds                  inz
018000050929      * - Ds di riferimento al file esterno AZUTE00F
018100050929     d AZUTEds       e ds                  extname(AZUTE00F)
018200050929      * - Ds per dati organigramma
018300050929     d DDatiUte      e ds
018400050929      *
018500120508     D FIDN20DS      E DS
018600981015     D Fidn22DS      E DS
018700981016     D Fidn00DS      E DS
018800120507     d FIDN40DS      e ds
018900981014     D FNLV50DS      E DS
019000990311     D DCCH          E DS
019100050331     D  UTE                   60    109       dim(5)                            utenti autorizzati
019200981230     D DDCT01        E DS
019300020201     D DDCF01        E DS
019400981014     D DSbs02        E DS                  EXTNAME(tibs02ds)
019500981014     D DSDN10        E DS                  EXTNAME(FIDN10DS)
019600981014     D TRUL06DS      E DS
019700981014     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
019800020128     D DGEDDN        E DS                  INZ
019900020128     D DSTD          E DS                  INZ
020000050218     d ds5a2         e ds
020100120508     d OG143         e ds                  inz
020200070404     d og148         e ds
020300070404     d dslr02        e ds
020400980521      *
020500980521     D KPJBA         E DS
020600980521      *
020700980521     D                SDS
020800980521     D  VTCPGM                 1     10
020900981015      *
021000981015     d dsnumca         ds
021100981015     d  aac                    1      4  0
021200981015     d  fil                    5      7  0
021300981015     d  nca                    8     14  0
021400050401
021500050401     D Wdataeur        S               D   DATFMT(*eur)
021600050331
021700050331     D ktpd            s                   LIKE(DCStpd)
021800050331     D knkt            s                   LIKE(DCSnkt)
021900050331     D kstd            s                   LIKE(DCSstd)
022000050331     D knks            s                   LIKE(DCSnks)
022100050331     D ktrc            s                   LIKE(DCStrc)
022200050331     D kdim            s                   LIKE(DCSdim)
022300050331     D khim            s                   LIKE(DCShim)
022400080114      *
022500080114     d Sav_In          s             99    inz(*zero)
022600980623
022700990122     c*---------------------------------------------------------------------------------------------
022800990122     c* se la causale di chiusura è valorizzata non emetto videate
022900990122     c*---------------------------------------------------------------------------------------------
023000990122     c* Pgm richiamato di chiusura con la causale di chiusura passante
023100990122     c                   if        i22tri = 'R' and i22mod = 'C' and
023200990122     c                             I22cch <> '  '
023300990122     c* valorizzo la chiave CA
023400990122     C                   z-add     V1Cfil        Kfil
023500990122     C                   z-add     V1Caac        Kaac
023600990122     C                   z-add     V1Cnca        Knca
023700990122     c*
023800990122     C     Keynca        chain     FNDCT000                           31
023900990122     c* faccio direttamente la chiusura della CA
024000990122     c                   exsr      SUB_coch
024100990122     c*
024200990122     c                   goto      fine
024300990122     c*
024400990122     c                   endif
024500990122     c*
024600990122     c* GESTIONE CHIUSURA RIAPERTURA A VIDEO
024700990122     c*
024800981013     C                   do        *hival
024900980623
025000980521      * Emissione VIDEO1
025100981013     c                   write     FI22t01
025200981013     c                   write     FI22z01
025300160913     c                   if        *in28
025400160913     c                   eval      *in28 = *off
025500160913     c                   write     FI22d01
025600160913     c                   eval      *in28 = *on
025700160913     c                   endif
025800981013     c                   exfmt     FI22d01
025900980923
026000980923     c                   setoff                                       284041
026100990419     c                   setoff                                       422190
026200980923     c                   clear                   v1cmsg
026300980623
026400980525      * f3=Fine
026500981021     c   kc              eval      o22f03 = 'S'
026600981013     c   kc              leave
026700980623
026800981014
026900980521      * f18=Cambio P.O. in gestione
027000980522     c   ks              eval      *in05 = *on
027100981013     c   ks              iter
027200980623
027300980521      * 05 on --> controllo P.O. in gestione
027400980521     c     *in05         ifeq      *on
027500980521     C                   exsr      ctrfgs
027600981013     c   28              iter
027700980608     c                   eval      *in05 = *off
027800980521     c                   end
027900980623
028000050512      * Controlli  videata - controllo C.A.
028100980521     c                   exsr      ctrd01
028200981105      * se è acceso il 21 o il 28 ritotrno ad emettere il formato video
028300050331     c                   if        *in28 or *in21 or *in90
028400981105     c                   iter
028500981105     c                   endif
028600981015      * f10=Note C.A.
028700981015     C     *INKJ         IFEQ      *ON
028800981015     c                   exsr      SUB_F10
028900981015     C                   iter
029000981015     c                   ENDIF
029100981015
029200981015      * f14=Interrogazione C.A.
029300981015     C     *INKN         IFEQ      *ON
029400981015     c                   exsr      SUB_F14
029500981015     C                   iter
029600981015     c                   ENDIF
029700050512      * Controlli  videata - altri controlli
029800050512     c                   exsr      CtrD01_b
029900050512      * se è acceso il 21 o il 28 ritotrno ad emettere il formato video
030000050512     c                   if        *in28 or *in21 or *in90
030100050512     c                   iter
030200050512     c                   endif
030300990511      * Se 20 acceso si tratta di una riapertura per rifiuto assegno di traenza
030400050331      * deve essere spento il 22
030500050331    1c                   IF        *in20 = *on  and *in22 = *off
030600990511      *   Se la C.A. resta in gestione alla sede imposto una fase fittizia x il ritorno al pagamento
030700990511    3c                   If        v1crit = 'S'
030800990511     c                   eval      wfaca = writi
030900990511     c                   Else
031000990511      *   Se deve tornare in gestione alla filiale richiamo il pgm rifiuto liquidazione transattiva
031100981105     c                   exsr      SUB_RIFIU
031200990511      *      x F12 riemetto videata, se confermata o premuto F3 esco
031300990511    4c                   If        o00f12 = 'S'
031400990312     c                   iter
031500990511    4c                   Else
031600990312     c                   leave
031700990511    4c                   Endif
031800990511      *
031900990511    3c                   Endif
032000990511    1c                   ENDIF
032100050331      *
032200050331      * se si tratta di un rifiuto assegno di traenza da causale 17 (22 acceso ) imposto
032300050331      * la fase 690 ristampa progetto di liquidazione)
032400050331     c                   If        *in22 = *on
032500050331     c                   eval      wfaca = wripl
032600050331     c                   endif
032700080114      *
032800080115      * Se chiusura con causale "Bonifico Manuale" richiede anche
032900080114      *   l'importo liquidato
033000080114     c                   clear                   $Video
033100080114if  1c                   IF        NOT  *in08
033200080115     c                             and  V1Ccch = WchBM
033300080114     c                   eval      $Video = 'D2'
033400080114do  2c                   dow       $Video = 'D2'
033500080114     c                   exsr      GesD02
033600080114e   2c                   enddo
033700080114if  2c                   if        O22f03 = 'S'
033800080114     c                   leave
033900080114e   2c                   endif
034000080114e   1c                   ENDIF
034100990511      *
034200981014      * Se non ho confermato o ci sono errori riemetto video
034300981015     c                   IF        *INKF = *ON
034400980910     c                   exsr      SUB_confer
034500981015     c                   leave
034600980908     c                   ELSE
034700981013     c                   iter
034800980908     c                   ENDIF
034900981014      *
035000981014     c                   enddo
035100980623
035200981021     c                   clear                   dsdn10
035300981021     c                   movel     'C'           i10tla
035400981021     c                   movel     dsdn10        kpjbu
035500981021     c                   call      'FIDN10R'
035600981021     c                   parm                    kpjba
035700981021      *
035800010508     c                   if        %OPEN(fidn22d)
035900990122     c                   close     fidn22d
036000010508     c                   endif
036100990122      *
036200990122     c     fine          tag
036300010508      *
036400981021     c                   movel     fidn22ds      kpjbu
036500981021
036600981029     c                   clear                   dsbs02
036700981029     c                   movel     'C'           t02tla
036800981029     c                   call      'TIBS02R'
036900981029     c                   parm                    kpjba
037000981029     c                   parm                    dsbs02
037100980907     c                   EVAL      *INLR = *ON
037200980521      **********************************************************************
037300980521      * CONTROLLO IL P.O. IN IN GESTIONE
037400980521      **********************************************************************
037500980521     C     CTRFGS        beGSR
037600980521      *
037700980521     c                   clear                   v1dfgs
037800980609     C                   clear                   FNLV50DS
037900980521     C                   MOVEL     KNMUS         D50PRU
038000980521     C                   MOVEL     V1cfgs        D50FGS
038100980521     C                   CALL      'FNLV50R'
038200980521     C                   PARM                    FNLV50DS
038300980521      *
038400980521     C                   if        D50ERR <> *blanks
038500980521     C                   MOVEL     D50MSG        V1cMSG
038600980923     C                   SETON                                        4028
038700980609     C                   else
038800070404     c                   clear                   og148
038900980609     c     v1cfgs        CHAIN     azorg                              31
039000980609     c  n31              MOVEL     orgdes        v1dfgs
039100070404     c  n31              eval      og148 = orgde8
039200070404      * controllo se Filiale gestione è abilitata a telefonate GEO
039300070404     c                   eval      wfilgeo = (§oggeot = 'S')
039400980910     C                   endif
039500980521      *
039600981014     C  n28              EXSR      CARL6
039700980609     c                   endsr
039800981014      **************************************************************************
039900981014      * CARICO SCHIERA L6 DA TABELLA £6
040000981014      **************************************************************************
040100981014     C     carl6         BEGSR
040200981014      *
040300981014     C                   CLEAR                   L6
040400981014     C                   CLEAR                   TRUL06DS
040500981014     C                   MOVE      '£6'          D06COD
040600981014     C                   MOVEL     v1cfgs        D06key
040700981014     C                   MOVEL(P)  TRUL06DS      KPJBU
040800981014     C                   CALL      'TRUL06R'
040900981014     C                   PARM                    KPJBA
041000981014     C                   MOVEL     KPJBU         TRUL06DS
041100981014     C                   MOVEA     LIN           L6
041200981014      *
041300981014     c                   endsr
041400980521      **********************************************************************
041500980521      * CONTROLLI VIDEO 1
041600980521      **********************************************************************
041700980923     C     CTRD01        BEGSR
041800980904
041900160913      * -?Spegnimento indicatori per visualizzazione dati della "PRATICA"?
042000160913     c                   eval      *in23 = *off
042100160913     c                   eval      *in24 = *off
042200160921     c                   eval      *in70 = *off
042300160914     c                   eval      *in71 = *off
042400160914     c                   eval      *in72 = *off
042500160914     c                   eval      *in73 = *off
042600160914     c                   eval      *in74 = *off
042700160921     c                   eval      *in75 = *off
042800160921     c                   eval      *in76 = *off
042900160913      * -?Pulizia campi con dati della "PRATICA"?
043000160913     c                   clear                   V1Cpli
043100160913     c                   clear                   V1Dpli
043200160913     c                   clear                   V1Cdds
043300160913     c                   clear                   V1Cdpl
043400160913     c                   clear                   V1Cipp
043500160913     c                   clear                   V1Cvpp
043600160913     c                   clear                   V1Cvlp
043700160921     c                   clear                   V1Odch
043800160921     c                   clear                   V1Occh
043900160921     c                   clear                   V1OcchD
044000160913      *
044100981013      * Se previsto controllo P.O. C.A.
044200981014    1C                   IF        V1CFIL = *zeros
044300981013     C                   movel     MSG(2)        V1Cmsg
044400981012     C                   seton                                        2840
044500981014    1C                   ENDIF
044600981012     c   28              goto      Ectrd01
044700981012
044800981013      * Controllo Anno C.A.
044900981014    1C                   IF        V1Caac = *zeros
045000981013     C                   movel     MSG(2)        V1Cmsg
045100980923     C                   seton                                        2841
045200981014   x1C                   ELSE
045300980923      *   sistemo anno di due cifre
045400981014    2C                   if        V1Caac < 100  and  V1Caac > 60
045500981013     C                   ADD       1900          V1Caac
045600981014    2C                   endif
045700981014    2C                   if        V1Caac < 100  and  V1Caac <= 60
045800981014     C                   ADD       2000          V1Caac
045900981014    2C                   endif
046000981014    1C                   ENDIF
046100980923     c   28              goto      Ectrd01
046200980923
046300981013      * Controllo N° C.A.
046400981014    1C                   IF        V1Cnca = *zeros
046500981013     C                   movel     MSG(2)        V1Cmsg
046600980923     C                   seton                                        2841
046700981014   x1C                   ELSE
046800980923      *
046900981014     c* valorizzo la chiave CA
047000981014     C                   z-add     V1Cfil        Kfil
047100981014     C                   z-add     V1Caac        Kaac
047200981014     C                   z-add     V1Cnca        Knca
047300981014     C     Keynca        CHAIN     FNDCT000                           31
047400981230      *
047500981230     c                   movel     dctflo        ddct01
047600981230     c                   move      §dctlnpc      W§dctlnpc
047700050218      * se dctdt2 > 0 e sono in filiale faccio finta che la c.a. non esista
047800050218     c                   If        dctdt2 > 0 and not *In07
047900050218     c                   Eval      *In31 = *On
048000050218     c                   EndIf
048100980904      *
048200981014    2C                   IF        *in31 = *on
048300980923     C                   movel     MSG(1)        V1Cmsg
048400980923     C                   seton                                        2841
048500981014   x2c                   else
048600981016     c* se esiste la CA ma non sono il po che l'ha in gestione ed ho già trasmesso il
048700981016     c* record non faccio fare la chiusura della CA
048800981030     c* non faccio questo controllo quando vengo richiamato
048900981030     c                   if        *in10 = *off
049000981014    3c                   if        v1cfgs <> dctgfc
049100981014     C* verifico se si trova nella sua £6
049200981014     c     dctgfc        lookup    L6                                     30
049300981014     c  n30              seton                                        2841
049400990712     C   28              movel     MSG(4)        V1Cmsg
049500990712     c   28              goto      Ectrd01
049600981014    3c                   ENDIF
049700981030     c                   endif
049800050512     c*
049900050512    2c                   ENDIF
050000050512    1C                   ENDIF
050100160913      *
050200160913      /free
050300160913
050400160913         // -?Eventuale visualizzazione estremi della Pratica?
050500160913         if  V1Ccch <> WchBB  or
050600160921             DCTfca <  wPrLi  or
050700160921             *in28  =  *on;
050800160913           leavesr;
050900160913         endif;
051000160913
051100160913         *in23 = *on;
051200160914
051300160914         // -?Pratica affidata ad A.I.G.?
051400160914         *in71 = (§DCTaffi = 'A');
051500160914         // -?Pratica Assicurativa?
051600160914         *in72 = (DCTfpr  <> 'T'  and  §DCTpFra <> 'F');
051700160914         // -?Liquidazione Transattiva?
051800160914         *in73 = (DCTfpr   = 'T'  and  §DCTpFra <> 'F');
051900160914         // -?Pratica in Franchigia?
052000160915         *in74 = (§DCTpFra = 'F');
052100160921
052200160921         // -?Eventuale visualizzazione ALERT x anno trascorso dalla fase 700?
052300160921         If  V1Ccch = WchBB  and  DCTfca = wPrLi;
052400160921
052500160921           setGT     ( Kaac : Kfil : Knca )  FNDCF000;
052600160921           readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
052700160921           DoW  NOT %eof( FNDCF01L)  and  DCFfca <> wPrLi;
052800160921             readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
052900160921           EndDo;
053000160921
053100160921           if  NOT %eof( FNDCF01L )  and  DCFfca = DCTfca;
053200160921             *in24 = ( DCFdfc <= %dec( %date() - %years(1) ) );
053300160921           endif;
053400160921
053500160921         EndIf;
053600160921
053700160921         // -?Eventuale visualizzazione dati della Chiusura?
053800160921         If  DCTfca = wChCA;
053900160921
054000160921           *in70 = *on;
054100160921
054200160921           if  DCTdch > *zero;
054300160921             WdataEUR = %date( DCTdch : *iso );
054400160921             V1Odch   = %dec( WdataEUR );
054500160921           endif;
054600160921
054700160921           if  DCTcch > *blank;
054800160921             V1Occh = DCTcch;
054900160921             clear  dsBS02;
055000160921             T02mod = 'C';
055100160921             T02cod = 'CCH';
055200160921             T02ke1 = DCTcch;
055300160921      /end-free
055400160921     c                   call      'TIBS02R'
055500160921     c                   parm                    KPJBA
055600160921     c                   parm                    dsBS02
055700160921      /free
055800160921             if  T02err = *blank;
055900160921               dCCH    = T02uni;
056000160921               V1OcchD = §CCHdesc;
056100160921             endif;
056200160921           endif;
056300160921           // -?Verifica se è già stata fatta almeno un'estrazione?
056400160921           *in75 = ( §DCTpaga = 'C' );
056500160921           // -?Verifica se è già stato fatto il pagamento?
056600160921           *in76 = ( §DCTpaga = 'P' );
056700160921
056800160921         EndIf;
056900160915
057000160915         if  Not *in71  and  Not *in72  and
057100160915             Not *in73  and  Not *in74;
057200160915           *in23 = *off;
057300160921           *in24 = *off;
057400160915           leavesr;
057500160915         endif;
057600160913
057700160914         // -?Beneficiario pagamento C.A.?
057800160914         //  ?(SE Liquidazione Transattiva o Pratica in Franchigia)?
057900160914         if  *in73  or  *in74;
058000160914           chain(N)  ( Kaac : Kfil : Knca : 'P' )  FNDKA000;
058100160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
058200160914             V1Cpli = DKAksc;
058300160914             V1Dpli = DKArag;
058400160914           endif;
058500160914         // -?Intestatario Progetto Liquidazione?
058600160914         else;
058700160914           chain(N)  ( Kaac : Kfil : Knca : 'I' )  FNDKA000;
058800160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
058900160914             V1Cpli = DKAksc;
059000160914             V1Dpli = DKArag;
059100160914           endif;
059200160914         endif;
059300160913
059400160913
059500160921         // -?Altri dati del Progetto di Liquidazione:?
059600160913         if  Not %open(FNDCL01L);
059700160913           open  FNDCL01L;
059800160913         endif;
059900160913         chain(N)  ( Kaac : Kfil : Knca )  FNDCL000;
060000160915
060100160915         If  %found(FNDCL01L);
060200160913
060300160915           // -?Data Stampa Progetto di Liquidazione?
060400160915           if  DCLdds > *zeros;
060500160915             WdataEUR = %date( DCLdds : *iso );
060600160915             V1Cdds   = %dec( WdataEUR );
060700160915           endif;
060800160915
060900160915           // -?Data Accettazione Progetto di Liquidazione?
061000160915           if  DCLdpl > *zeros;
061100160915             WdataEUR = %date( DCLdpl : *iso );
061200160915             V1Cdpl   = %dec( WdataEUR );
061300160915           endif;
061400160915
061500160915           // -?Importo Liquidato?
061600160915           if  DCTfpr = 'T';
061700160915             //*·V2Cipl = DCLipl;
061800160915             //*·V2Cvpl = DCLvpl;
061900160915           else;
062000160915             V1Cipp = DCLipl;
062100160915             V1Cvpp = DCLvpl;
062200160915             //*·// -?Se pratica trattenuta da cliente e?
062300160915             //*·//     ?pagamento già avvenuto?
062400160915             //*·//  ?=> imposto come importo liquidato la differenza?
062500160915             //*·if  DCLipl <= DCLipt;
062600160915             //*·  clear  V2Cipl;
062700160915             //*·  clear  V2Cvpl;
062800160915             //*·else;
062900160915             //*·  V2Cipl = DCLipl - DCLipt;
063000160915             //*·  V2Cvpl = DCLvpl;
063100160915             //*·endif;
063200160915           endif;
063300160915
063400160915           // -?Data valuta pagamento?
063500160915           if  DCLvlp > *zeros;
063600160915             WdataEUR = %date( DCLvlp : *iso );
063700160915             V1Cvlp   = %dec( WdataEUR );
063800160915           endif;
063900160915
064000160915         EndIf;
064100160913
064200160913      /end-free
064300050512
064400050512     c     Ectrd01       ENDSR
064500050512      **********************************************************************
064600050512      * Altri CONTROLLI VIDEO 1
064700050512      **********************************************************************
064800050512     C     CtrD01_b      BEGSR
064900020128      *
065000020128      * Verifico se C.A. con flag di franchigia = 'F'
065100020128      *                  e  causale di chiusura = '15'
065200020128     c                   if            §DCTpfra = 'F'
065300020226     c                             and DCTcch   = WchFR
065400020128     c                   eval      $Franch = *on
065500020128     c                   else
065600020128     c                   eval      $Franch = *off
065700020128     c                   endif
065800981016     c*
065900981016    3c                   if        *in08 = *off
066000981016     c* controllo nel caso di chiusura CA
066100981016     c                   exsr      SUB_ctrch
066200981016   x3c                   else
066300981016     c* controllo nel caso di riapertura CA
066400981016     c                   exsr      SUB_ctrri
066500981016    3c                   endif
066600050512     c   28              goto      Ectrd01b
066700980923
066800050512     c     Ectrd01b      ENDSR
066900981016      ****************************************************************
067000981016      * SUB_CTRCH Controlli di chiusura CA
067100981016      ****************************************************************
067200981016     C     SUB_CTRCH     BEGSR
067300981016      *
067400981016     c* controllo se C.A. già chiusa
067500981016    1c                   if        dctdch > 0
067600981016     c                   seton                                        2841
067700981016     C                   movel     MSG(5)        V1Cmsg
067800981016    1c                   ENDIF
067900981016      *
068000981016      * Controllo la causale di chiususra
068100981016    1c                   if        *in28 = *off
068200981030    2c                   if        v1ccch = '  '
068300981016     c                   seton                                        4228
068400981016     c                   movel     msg(3)        v1cmsg
068500981016     c                   goto      ech01
068600981016    2c                   endif
068700050126      * solo la sede
068800050511    2c                   if           (V1Ccch = WchBA
068900050511     c                             or  V1Ccch = WchBB)
069000050511     c                             and *in07  = *off
069100050126     c                   seton                                        4228
069200050126     c                   movel     msg(16)       v1cmsg
069300050126     c                   goto      ech01
069400050126    2c                   endif
069500050512      *
069600050512      * SOLO IN SEDE controllo la causale di spedizione 17/18
069700050512  1.5c                   if        *in07
069800050511      * reperisco la data spedizione
069900050511     c                   exsr      ChaTITAS
070000050511      * causale chiusura 17 errata se data spedizione >  17/04/2005
070100050511    2c                   if            (DCTaas * 10000 + TASmgs)
070200050511     c                              >  C_DataChiusLim
070300050511     c                             and V1Ccch = WchBA
070400050511     c                   seton                                        2842
070500050512     c                   eval      V1Cmsg = %replace('precedente il' :
070600050511     c                                      MSG(18) :
070700050512     c                                      %scan('&PRE/SUC':MSG(18)))
070800050511     c                   goto      ech01
070900050511    2c                   endif
071000050511      * causale chiusura 18 errata se data spedizione <= 17/04/2005
071100050511    2c                   if            (DCTaas * 10000 + TASmgs)
071200050511     c                              <= C_DataChiusLim
071300050511     c                             and V1Ccch = WchBB
071400050511     c                   seton                                        2842
071500050512     c                   eval      V1Cmsg = %replace('successiva al' :
071600050511     c                                      MSG(18) :
071700050512     c                                      %scan('&PRE/SUC':MSG(18)))
071800050511     c                   goto      ech01
071900050511    2c                   endif
072000050512  1.5c                   endif
072100050512      *
072200981016      * controllo la causale di chiusura
072300981016    2c                   if        v1ccch <> '  '
072400981016      *
072500981016     C     '?'           SCAN      v1Ccch                                 31
072600981016      *
072700981016    3C                   IF        *IN31 = *ON
072800990419     c                   seton                                        90
072900981016     c                   clear                   dsbs02
073000981016     c                   movel     'R'           t02mod
073100981016     c                   movel     knsif         t02sif
073200981016     C                   movel     'CCH'         t02cod
073300981016      *
073400981016     C                   CALL      'TIBS02R'
073500981016     c                   parm                    KPJBA
073600981016     c                   parm                    dsbs02
073700981016      *
073800981016    4c                   if        t02err =  *blanks
073900981016     C                   MOVEL     t02ke1        v1ccch
074000981016    5C                   IF        v1ccch <> *BLANKS
074100981016     c                   movel     t02uni        v1dcch
074200990311     c                   clear                   dcch
074300990311     C                   MOVEL     T02UNI        dcch
074400990311     c* verifico se causale si può usare a video
074500990311     c                   if        §cchgevi <> 'S'
074600990311     c                   movel     msg(10)       v1cmsg
074700990311     c                   seton                                        4228
074800990311     c                   goto      ech01
074900990311     c                   endif
075000990311     c*
075100981016    5C                   ENDIF
075200981030     c                   else
075300981030     c                   movel     t02msg        v1cmsg
075400981030     c                   goto      ech01
075500981016    4c                   endif
075600981016      *
075700981016     C                   GOTO      Ech01
075800981016   x3C                   Else
075900981016      *   Controlli di validità'
076000981016      *
076100981016     c                   clear                   dsbs02
076200981016     C                   MOVEL     'C'           t02mod
076300981016     C                   MOVEL     knsif         t02sif
076400981016     C                   MOVEL     'CCH'         t02cod
076500981016     C                   MOVEL(P)  v1ccch        t02ke1
076600981016      *
076700981016     C                   CALL      'TIBS02R'
076800981016     C                   PARM                    KPJBA
076900981016     C                   PARM                    dsbs02
077000981016      *
077100981016    4C                   if        t02err = *BLANKS
077200981016     C                   MOVEL     T02UNI        v1dcch
077300990311     c                   clear                   dcch
077400990311     C                   MOVEL     T02UNI        dcch
077500990311     c* verifico se causale si può usare a video
077600990311     c                   if        §cchgevi <> 'S'
077700990311     c                   movel     msg(10)       v1cmsg
077800990311     c                   seton                                        4228
077900990311     c                   goto      ech01
078000990311     c                   endif
078100990311     c*
078200981016     c                   else
078300990311     c*
078400981016     c                   movel     msg(3)        v1cmsg
078500981016     c                   seton                                        4228
078600981016     c                   goto      ech01
078700981016    4C                   endif
078800981016    3C                   endif
078900981016     c                   else
079000981016     c                   clear                   v1dcch
079100981016    2C                   endif
079200981016    1C                   endif
079300050126     c* controllo se C.A. in fase minore del progetto di liquidazione
079400050511    1c  n28              if             DCTfca < Wprli
079500050511     c                             and (V1Ccch = WchBA
079600050511     c                              or  V1Ccch = WchBB)
079700050126     c                   seton                                        2842
079800050126     C                   movel     MSG(15)       V1Cmsg
079900050929     c                   leavesr
080000050126    1c                   ENDIF
080100050929      *
080200050929      * controlli sull'utilizzo della causale in base ai flag in DCCH:
080300050929      * - per le causali di pagamento: ci deve essere compatibilità
080400131011      *   con l'affidamento (o meno) ad A.I.G. già memorizzato su
080500050929      *   FNDCT00F (in DCTFLO - ds DDCT01)
080600111223if  1c****               if            §CCHpaga  = 'S'
080700111223     c****                         and §DCTaffi  = 'A'
080800111223     c****                         and V1Ccch   <> WchBB
080900111223     c****               eval      V1Cmsg   = %trim(Msg(20))
081000111223     c****               seton                                        2842
081100111223     c****               leavesr
081200111223e   1c****               endif
081300050929      * - per le causali utilizzabili solo in sede: verifico la
081400050929      *   compatibilità con il profilo utente
081500051025sel 1c                   select
081600050929      * - - utilizzabile da tutti gli utenti
081700051025w   1c                   when      §CCHutil = *blanks
081800050929      * - - utilizzabile dai soli utenti di sede
081900051025w   1c                   when          §CCHutil =  'S'
082000050929     c                             and DUTlpo   <> 'S'
082100050929     c                   eval      V1Cmsg = %trim(Msg(21))
082200050929     c                                    + ' sede'
082300050929     c                   seton                                        2842
082400050929     c                   leavesr
082500050929      * - - utilizzabile dai soli utenti di p.o.
082600051025w   1c                   when          §CCHutil =  'P'
082700050929     c                             and DUTlpo   =  'S'
082800050929     c                   eval      V1Cmsg   = %trim(Msg(21))
082900080114     c                                      + ' filiale'
083000050929     c                   seton                                        2842
083100050929     c                   leavesr
083200051025e   1c                   endsl
083300981130     c*
083400981016
083500981016     c     ech01         ENDSR
083600981016      ****************************************************************
083700981016      * SUB_CTRRI Controlli di riapertura CA
083800981016      ****************************************************************
083900981016     C     SUB_CTRRI     BEGSR
084000981016      *
084100981016     c* controllo se C.A. non chiusa  in caso di riapertura C.A.
084200981130    1c                   if        dctdch = 0
084300981016     c                   seton                                        2841
084400981016     C                   movel     MSG(6)        V1Cmsg
084500981016     c                   goto      eri01
084600981016    1c                   ENDIF
084700050922      *
084800131011     c* controllo se C.A. già in fase di richiesta recupero ad A.I.G.
084900050922     C     Keynca        chain     FNDRA01L
085000050922    1c                   if        %found(fndra01l) and dradrr > 0
085100050922     c                   seton                                        2841
085200050922     C                   movel     MSG(19)       V1Cmsg
085300050922     c                   goto      eri01
085400050922    1c                   ENDIF
085500990712     c*
085600990712     c* calcolo la data massima per riapertura CA
085700990712     c     *iso          move      dctdch        wdata
085800050218     c**!!!              adddur    4:*m          wdata
085900050218     c                   adddur    §5abca:*d     wdata
086000990712     c     *iso          move      wdata         wrkdta
086100010926     c*
086200010926     c* se non riapribile in modo normale:
086300050218     c* solo in sede: riapertura consentita oltre i giorni da tabella 5A
086400010926     c*           ma  non se bolla antecedente il 1° settembre 1999.
086500010926     c* (modifico la data massima per riapertura CA - wrkdta)
086600010926     c                   eval      $over4m = *off
086700011008    1c                   if        *in07         and
086800010926     c                             dateu   > wrkdta
086900010926     c                   eval      $over4m = *on
087000011008     c                   exsr      ChaTITAS
087100011008     c* ok oltre il 01/09/1999
087200011008    2c                   if         DCTaas  > 1999   or
087300011008     c                             (DCTaas  = 1999  and  TASmgs >= 0901)
087400011008     c                   eval      wrkdta  = *hival
087500011008     c                   else
087600011008     c                   seton                                        2841
087700011008     c                   movel     MSG(8)        V1Cmsg
087800011008     c                   goto      eri01
087900011008    2c                   endif
088000011008    1c                   endif
088100010926     c*
088200050218     c* controllo se CA chiusa da più giorni da tab. 5A
088300990712     c                   if        dateu > wrkdta
088400990712     c                   seton                                        2841
088500050218     c                   move      §5abca        w003a             3
088600050218     C                   movel     MSG(14)       V1Cmsg
088700050218     c                   Eval      %subst(v1cmsg:48:3) = w003a
088800990712     c                   goto      eri01
088900990712    1c                   endif
089000990712     c
089100990311     c* controllo se la C.A. chiusa  non sia in fase di contabilizzazione
089200990311    1c                   if        §dctpaga = 'C'
089300990311     c                   seton                                        2841
089400990311     C                   movel     MSG(11)       V1Cmsg
089500990311     c                   goto      eri01
089600990311    1c                   endif
089700050331      * verifico se la CA chiusa può essere riaperta solo da un utente autorizzato
089800050331      *
089900050331     c                   clear                   dsbs02
090000050331     C                   MOVEL     'C'           t02mod
090100050331     C                   MOVEL     knsif         t02sif
090200050331     C                   MOVEL     'CCH'         t02cod
090300050331     C                   MOVEL(P)  dctcch        t02ke1
090400050331      *
090500050331     C                   CALL      'TIBS02R'
090600050331     C                   PARM                    KPJBA
090700050331     C                   PARM                    dsbs02
090800050331      *
090900050331     c                   clear                   dcch
091000050331     C                   MOVEL     T02UNI        dcch
091100050331      * se richiesto il controllo dell'utente escludo gli utenti EDP
091200050331     c                   If        §cchprot = 'S' and %subst(knmus:1:3)<>'EDP'
091300050331      *
091400160914     c     knmus         lookup    ute                                    33
091500050331     c                   if        not %found
091600050331     c                   seton                                        2841
091700050331     C                   movel     MSG(17)       V1Cmsg
091800050331     c                   goto      eri01
091900050331    1c                   endif
092000050331    1c                   endif
092100990311     c* controllo se C.A. in fase di pagamento non deve essere per pratica assicurativa
092200050420    1c                   if        §dctpaga = 'P'  and
092300050331     c                             (dctcch = wchpa )
092400050420     c*                  seton                                        2841
092500050420     C*                  movel     MSG(12)       V1Cmsg
092600050420     c*                  goto      eri01
092700990311    1c                   endif
092800990311     c* Controllo se il numero dei colli in testata CA è uguale al numero dei colli nel dettaglio
092900990311     c                   exsr      SUB_COLLI
093000990311     c*
093100990311     c* se numero colli differenti non si può aprire
093200990311    1c                   if        dctncn <> wnco
093300990311     c                   seton                                        2841
093400990311     C                   movel     MSG(13)       V1Cmsg
093500990311     c                   goto      eri01
093600990311    1c                   endif
093700981016     c*
093800981016     c* controllo chi sono io
093900981230     c                   if        dctgfc = W§dctlnpc
094000981016     c                   movel     'P'           chisono                        * PARTENZA
094100040722     c                   eval      giaaas = dctaas
094200040722     c                   eval      gialnp = dctlnp
094300040722     c                   eval      gianrs = dctnrs
094400040722     c                   eval      giansp = dctnsp
094500981016     c                   endif
094600981016     c                   if        dctgfc = dctlna
094700981016     c                   movel     'A'           chisono                        * ARRIVO
094800040722      * verifico se ho spedizione figlia su FNDCK prendo quella
094900040722     c     keynca        chain     fndck01l
095000040722     c                   if        %found(fndck01l)
095100040722     c                   eval      giaaas = dckaas
095200040722     c                   eval      gialnp = dcklnp
095300040722     c                   eval      gianrs = dcknrs
095400040722     c                   eval      giansp = dcknsp
095500040722     c                   else
095600040722     c                   eval      giaaas = dctaas
095700040722     c                   eval      gialnp = dctlnp
095800040722     c                   eval      gianrs = dctnrs
095900040722     c                   eval      giansp = dctnsp
096000040723     c                   endif
096100040723      *
096200981016     c                   endif
096300981016     c                   if        dctgfc = 046
096400981016     c                   movel     'S'           chisono                        * SEDE
096500981016     c                   endif
096600981016     c*
096700040722     c* sono l' ARRIVO o la PARTENZA controllo la giacenza in ARRIVO
096800040722     c                   if        chisono <> 'S'
096900010508      *
097000050404     C                   if        not %OPEN(Tigcp21l)
097100050404     C                   OPEN      Tigcp21l
097200010508     c                   endif
097300010508      *
097400050404     C     kgiac         chain     Tigcp21l                           32
097500050404     c                   if        not *in32 and gcpfas < 40
097600981016     c                   seton                                        2841
097700981021     C                   movel     MSG(7)        V1Cmsg
097800981016     c                   goto      eri01
097900981016     c                   endif
098000010508      *
098100050404     C                   if        %OPEN(Tigcp21L)
098200050404     c                   close     Tigcp21l
098300010508     c                   endif
098400010508      *
098500010508      * controllo la data consegna e il numero Distinta consegna
098600010508     C                   if        not %OPEN(FNARB01L)
098700010508     C                   OPEN      FNARB01L
098800010508     c                   endif
098900010508      *
099000990311     C     karb          chain     fnarb01l                           32
099100981230     c* controllo se c'è un flag blocco di spedizione diverso da "D", da " " e da "*"
099200981230     c                   if        arbfbc <> *blanks  and  arbfbc <> '*'
099300981021     c                   movel     msg(9)        V1Cmsg
099400981019     c                   seton                                        2841
099500981019     c                   goto      eri01
099600981019     c                   endif
099700010508      *
099800010508     C                   if        %OPEN(FNARB01L)
099900010508     C                   close     FNarb01L
100000010508     c                   endif
100100010508      *
100200981016     c                   endif
100300981016     c*
100400981019      * SE NON CI SONO ERRORI
100500981019     c* cerco la fase precedente la chiusura CA
100600981202     c     keynca        setgt     fndcf01l
100700981019     c     keynca        readpe    fndcf01l                               31
100800981202     c                   dow       dcffca = wchca
100900981019     c     keynca        readpe    fndcf01l                               31
101000981202     c                   enddo
101100981130     c  n31              eval      wfaca = dcffca
101200981105     c*
101300981105     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
101400990511     c* liquidazione transattiva : se è così imposto se si tratta di un rifiuto assegno di traenza
101500990311     c                   if        chisono  = 'S'  and  §dctpaga = 'P' and
101600990511     c                             dcffca = wcolt  and  *in20 = *off
101700981105     c                   seton                                        2021
101800981105     c                   endif
101900990430     c*
102000990430     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
102100090925     c* liquidazione transattiva : se è così non imposto + la fase di esito ma quella di av.danno
102200090925     c* liquidazione transattiva
102300990430     c                   if        chisono  = 'S'  and  §dctpaga = ' ' and
102400990511     c                             dcffca = wcolt  and  *in20 = *off
102500090925     c*******            eval      wfaca  = wesit
102600090925     c                   eval      wfaca  = wavdt
102700990430     c                   endif
102800050331      * se sto riaprendo una CA pagata in surroga visualizzo i dati dell'assegno
102900050511     c                   if            (DCTcch   = WchBA
103000051025     c                              or  DCTcch   = WchBB)
103100050511     c                             and  §DCTpaga = 'P'
103200050511     c                             and  *in20    = *off
103300050331     c                   seton                                        202122
103400050331     c                   endif
103500050401      * per 21 acceso visualizzo gli estremi del pagamento
103600050401      *
103700050401     c                   If        *in21
103800050401
103900050401     C                   if        not %OPEN(Fndcl01L)
104000050401     c                   open      fndcl01l
104100050401     c                   endif
104200050401      *
104300050401     c     keynca        chain(n)  fndcl01l
104400050401     c                   If        %found(fndcl01l)
104500050401      * Data emissione Assegno
104600050401     c                   if        DCLdea > *zeros
104700050401     c     *iso          move      DCLdea        Wdataeur
104800050401     c     *eur          movel     Wdataeur      V1Cdea
104900050401     c                   endif
105000050401      *
105100050401      * Numero  Assegno
105200161206      * o Bonifico
105300050401     c                   movel     dclasn        v1casn
105400161206     c                   clear                   V1Dasn
105500161206     c                   SELECT
105600161206     c                   WHEN      dclasn = *blanks
105700161206     c                   eval      V1Dasn = 'Ass/Bon'
105800161206     c                   WHEN      %subst(dclasn:1:1) = 'B'
105900161206     c                   eval      V1Dasn = 'Bonifico'
106000161206     c                   OTHER
106100161206     c                   eval      V1Dasn = 'Assegno'
106200161206     c                   ENDSL
106300050401      * importo liquidato
106400050401     c                   z-add     DClipl        V1Cipl
106500050401     c                   movel     dclvpl        v1cvpl
106600050401
106700050401     c                   endif
106800050401
106900050401     c                   if        %OPEN(Fndcl01L)
107000050401     c                   close     fndcl01l
107100050401     c                   endif
107200050401
107300050401     c                   endif
107400120507
107500120507      *?Controllo se si sta riaprendo un Porto Franco Import EEX chiuso con CAU 06
107600120507     c                   eval      wPFIEEX = *off
107700120507     c                   IF        §DCTport = 'F' and §DCTtisp = 'I' and
107800120507     c                              DCTcch = wCHEE
107900120507     c                   eval      wPFIEEX = *on
108000120507      *?Recupero la data della bolla
108100120507     c                   exsr      ChaTITAS
108200120507     c                   eval      Wdaspe = (TASaas * 10000) + TASmgs
108300120507     c                   ENDIF
108400981016
108500981016     c     eri01         ENDSR
108600981014      ****************************************************************
108700981014      * F10 = GESTIONE NOTE
108800981014      ****************************************************************
108900981014     C     SUB_F10       BEGSR
109000981014      *
109100981015     c*
109200981015     c                   eval      aac = kaac
109300981015     c                   eval      fil = kfil
109400981015     c                   eval      nca = knca
109500981015     c*
109600981014     C                   clear                   DSDN10
109700981015      * calcolo data apertura C.A.
109800981015     C                   eval      wdataca = (DCTaac * 10000) + DCTmgc
109900981015     c*
110000981015     C                   CLEAR                   WLBDAT
110100981015     C                   Z-ADD     wdataca       G02inv
110200981015     C                   movel     '3'           G02err
110300981015     C                   CALL      'XSRDA8'
110400981015     C                   PARM                    WLBDAT
110500981015     c                   z-add     g02dat        wdataca
110600981015     c*
110700981015     c                   eval      i10dta = wdataca
110800981014     c                   movel     'N'           i10trc
110900981014     c                   movel     'C'           i10tpd
111000981014      *
111100080115     c                   if        $Video = 'D2'  and  *inKF
111200080115     c                   eval      I10flm = 'V'
111300080115     c                   else
111400981015     c                   movel     'M'           i10flm
111500080115     c                   endif
111600981130     c                   movel     wchca         i10nks
111700981014     c                   movel     dsnumca       i10nkt
111800981014      *
111900981014     c                   movel(P)  dsdn10        kpjbu
112000981014     C                   CALL      'FIDN10R'
112100981014     C                   PARM                    kpjba
112200080115     c                   movel     kpjbu         dsdn10
112300981014      *
112400981014     c                   ENDSR
112500981015      ****************************************************************
112600981015      * F14 = INTERROGAZIONE C.A.
112700981015      ****************************************************************
112800981015     C     SUB_F14       BEGSR
112900981016      *
113000981016     c                   clear                   FIDN00DS
113100981016      *
113200981016     c                   movel     'I'           I00mod
113300981130     c                   movel     wchca         I00fca
113400981016     c                   z-add     dctgfc        I00fgs
113500981016     c                   z-add     dctaac        I00aac
113600981016     c                   z-add     dctfil        I00fil
113700981016     c                   z-add     dctnca        I00nca
113800981016     c                   z-add     dctaas        I00aas
113900981016     c                   z-add     dctlnp        I00lnp
114000981016     c                   z-add     dctnrs        I00nrs
114100981016     c                   z-add     dctnsp        I00nsp
114200981016     c                   z-add     dctlna        I00lna
114300981016     c                   movel     dcttad        I00tad
114400981230     c                   if        dctgfc = W§dctlnpc
114500981016     c                   movel     'P'           I00tpb
114600981016     c                   endif
114700981016     c                   if        dctgfc = dctlna
114800981016     c                   movel     'A'           I00tpb
114900981016     c                   endif
115000981016     c                   if        dctgfc = 046
115100981016     c                   movel     'S'           I00tpb
115200981016     c                   endif
115300981016      *
115400981016     c                   movel     FIDN00DS      kpjbu
115500981016      *
115600981016     c                   call      'FIDN01R'
115700981016     c                   parm                    kpjba
115800981016      *
115900981015     c                   endsr
116000990311      *****************************************************************
116100990311      * CONTROLLO DETTAGLIO COLLI
116200990311      *****************************************************************
116300990311     C     SUB_COLLI     BEGSR
116400990311
116500990311      * Lettura del dettaglio colli per verificare se uguale a quello indicato
116600990311      * in testata
116700990311     c                   clear                   wnco
116800990311     c*
116900990311     c     keynca        setll     fndcd01l
117000990311     c     keynca        reade(n)  fndcd01l                               32
117100990311      *
117200990311     c                   dow       not *in32
117300990311      *
117400990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
117500990412     c* e record non annullato
117600990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
117700990412     c                             and dcdatb = ' '
117800990311     c                   add       1             wnco
117900990311     c                   endif
118000990311q    c*
118100990311     c     keynca        reade(n)  fndcd01l                               32
118200990311     c*
118300990311     c                   enddo
118400990311     c*
118500990311     c                   endsr
118600980608      *****************************************************************
118700980923      * AGGIORNO ARCHIVI
118800980608      *****************************************************************
118900980923     C     SUB_confer    BEGSR
119000981007
119100981015      * note fase ca
119200080115      * (se NON già confermate in subr. "GesD02")
119300080115if  1c                   IF             *in08
119400080115     c                             or   V1Ccch <> WchBM
119500981015     C                   CLEAR                   DSDN10
119600981015     c                   z-add     Wdataca       i10dta
119700981015     C                   movel     'C'           I10FLM
119800080115     c                   eval      I10tpd = 'C'
119900981015     c                   movel(P)  dsnumca       i10nkt
120000981015     c                   movel     Wchca         i10nks
120100981015     c                   movel     'N'           i10trc
120200981015     c                   movel(P)  dsdn10        kpjbu
120300981015     C                   CALL      'FIDN10R'
120400981015     C                   PARM                    kpjba
120500080115e   1c                   ENDIF
120600050404      *
120700050404     c* chiusura C.A.
120800050404     c                   if        *in08 = *off
120900050404     c                   exsr      sub_coch
121000050404     c*
121100050404     c                   else
121200050404     c* riapertura
121300050404     c                   exsr      sub_cori
121400050404     c                   endif
121500981009      *
121600980923     c                   ENDSR
121700981014      *****************************************************************
121800981014      * AGGIORNO ARCHIVI   CHIUSURA C.A.
121900981014      *****************************************************************
122000981014     C     SUB_coch      BEGSR
122100020129      *
122200020129     c                   movel     DCTflo        DDCT01
122300020226      *
122400120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
122500120723     c                   clear                   §DCTspgd
122600120723     c                   movel     DDCT01        DCTflo
122700120723     c                   clear                   DCTpgd
122800981014      *
122900981014      * Imposto campi per trasmissione dati
123000981019     c                   z-add     dateu         DCTdch
123100981130     c                   z-add     wfaca         DCtfca
123200981014     c                   move      v1ccch        DCTcch
123300981222     c                   clear                   DCTft1
123400981222     c                   clear                   DCTft2
123500981014      *
123600981014      * Aggiorno testata C.A.
123700981014     c                   EXCEPT    UPDdct
123800981014      *
123900981014      * Aggiorno l dettaglio C.A.
124000981014     c                   EXSR      UPDDCD
124100050126      *
124200050511      * se sto chiudendo una pratica con causale 17 o 18 aggiorno FNDCL
124300080115      *                                            (o 19)
124400080114     c                   if            V1Ccch = WchBA
124500050511     c                             or  V1Ccch = WchBB
124600080115     c                             or  V1Ccch = WchBM
124700050126     c                   exsr      UPDDCL1
124800050126     c                   endif
124900981014      *
125000981014      * Scrivo fasi C.A.
125100981014     c                   EXSR      WRTDCF
125200051124      * Se programma non richiamato  e
125300131011      * Se chiusa C.A. legata ad Evento con data segnalazione ad A.I.G.
125400131011      *                               o in fase di rimborso A.I.G.
125500131011      *         o C.A. già affidata ad A.I.G. (con importo > §STDLAE)
125600131011      * => emetto window con msg: "C.A. per cui chiedere recupero A.I.G."
125700051018      *                           "Fotocopiare progetto liquidazione "
125800051124     c                   if        i22tri = '  '
125900080114      *
126000051124     c                   if        DCTnev   > *zeros
126100051020     c     K02DET01      chain(n)  FNDET000
126200051020     c                   if        NOT %found(FNDET01L)
126300051020     c                             or  DETatb <> *blanks
126400051020     c                   clear                   DETdtr
126500051020     c                   clear                   DETftr
126600051020     c                   endif
126700051117     c                   endif
126800051020     c                   if             DETdtr   > *zeros
126900051018     c                              or  DETftr   = 'S'
127000051020     c                              or  §DCTaffi = 'A'
127100051018     c                   exfmt     FI22W01
127200051018     c                   endif
127300981014      *
127400051124     c                   endif
127500070404
127600070404      * se filiale gestione non è sede
127700070404    1c                   if        v1cfgs <> 046
127800070404      * se filiale gestione è abilitata alle telefonate GEO
127900070404    2c                   if        wfilgeo = *on
128000070404      * se LNA della spedizione in £6
128100070404     c     dctlna        lookup    l6                                     31
128200070404    3c                   if        *in31
128300070404      * controllo la data consegna merce e la distinta consegna
128400070404     c                   if        not %open(fnarb01l)
128500070404     c                   open      fnarb01l
128600070404     c                   endif
128700070404     c     karb          chain(n)  fnarb01l
128800070404    4c                   if        %found(fnarb01l) and
128900070404     c                             arbndc = *zeros and arbdcm = *zeros
129000070404     c                   clear                   dslr02
129100070404     c                   eval      dr2aas = dctaas
129200070404     c                   eval      dr2lnp = dctlnp
129300070404     c                   eval      dr2nrs = dctnrs
129400070404     c                   eval      dr2nsp = dctnsp
129500070404     c                   eval      dr2fgs = v1cfgs
129600070404     c                   eval      dr2ric = '1'
129700070404     c                   eval      kpjbu = dslr02
129800070404     c                   call      'FNLR39R'
129900070404     c                   parm                    kpjba
130000070404     c                   eval      dslr02 = kpjbu
130100070404    4c                   endif
130200070404     c                   if        %open(fnarb01l)
130300070404     c                   close     fnarb01l
130400070404     c                   endif
130500070404    3c                   endif
130600070404    2c                   endif
130700070404    1c                   endif
130800051124      *
130900981014     c                   ENDSR
131000981014      *****************************************************************
131100981014      * AGGIORNO ARCHIVI   RIAPERTURA C.A.
131200981014      *****************************************************************
131300981014     C     SUB_cori      BEGSR
131400990311      *
131500990311      * Aggiorno il dettaglio C.A. che ha la data e causale chiusura uguale a quella della testata
131600990311     c     keynca        setll     fndcd01l
131700990311     c     keynca        reade     fndcd01l                               32
131800990311      *
131900990311     c                   dow       not *in32
132000990311      *
132100990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
132200990311     c*
132300990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
132400990311     c                   clear                   DCddch
132500990311     c                   clear                   DCdcch
132600990311     c                   clear                   DCdftr
132700990311     c                   update    fndcd000
132800990311     c                   endif
132900990311q    c*
133000990311     c     keynca        reade     fndcd01l                               32
133100990311     c*
133200990311     c                   enddo
133300120507      *?Se riapertura di un Porto Franco Import EEX chiuso con CAU 06
133400120507     c                   IF        wPFIEEX
133500120507      *?Cerco il codice del partner da impostare come beneficiario alternativo
133600120507     c                   clear                   FIDN40DS
133700120507     c                   eval      I40dsp = Wdaspe
133800120507     c                   eval      I40fil = dctlnp
133900120507     c                   call      'FIDN40R'
134000120507     c                   parm                    fidn40ds
134100120507      *?Ho trovato il codice del partner imposto la Fase e il beneficiario alternativo
134200120507     c                   IF        O40cod <> *zeros
134300120507     c                   eval      wfaca = wAVVD
134400120507     c                   eval      DCTksc = O40cod
134500120508      *?Devo anche recuperare il numero di pratica
134600120508     c                   IF        DCTprn = *zeros
134700120508     c                   exsr      NUMPRN
134800120508     c                   ENDIF
134900120509     c                   ENDIF
135000120507     c                   ENDIF
135100020131      *
135200981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
135300080123     c                   eval      wSAVcch = DCTcch
135400981014     c                   z-add     *zeros        DCTdch
135500981130     c                   z-add     wfaca         DCtfca
135600080701      * verifico se CA in fase 200 ormai obsoleta da Giugno 2008 imposto la filiale di gestione
135700080701      * in 046 sede per far eseguire la fase 230
135800080701     c                   if        dctfca = 200
135900080701     c                   eval      dctgfc = 46
136000080701     c                   endif
136100080701
136200981014     c                   clear                   DCTcch
136300981222     c                   clear                   DCTft1
136400981222     c                   clear                   DCTft2
136500120723      *
136600120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
136700120723     c                   clear                   DCTpgd
136800120723     c                   clear                   §DCTspgd
136900120723     c                   movel     DDCT01        DCTflo
137000020131      *
137100050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
137200020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
137300020131      *   causale "15"
137400020131      * => pulisco i flag per l'estrazione
137500050420      *   ADESSO PULISCO SEMPRE ***
137600990430      *
137700050420     c***                if        wfaca = writi or wfaca = wripl
137800050420    1c****                         or $Franch = *on
137900990430     c* pulisco il flag dctpaga nel campo dctflo
138000990430     c                   clear                   §dctpaga
138100990430     c                   movel     ddct01        dctflo
138200050420     c*****              endif
138300050218
138400050218      * Se sono decorsi i gg. da tab. 5A dalla data di chiusura
138500050218      * pulisco il campo dctdt2
138600050218    2c                   if        $over4m = *on
138700050218     c                   Clear                   dctdt2
138800050218    2c                   endif
138900020131      *
139000990430      * Aggiorno testata C.A.
139100990430     c                   EXCEPT    UPDdct
139200020131      *
139300050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
139400020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
139500020131      *   causale "15"
139600020131      * => pulisco i flag per l'estrazione
139700990430      *
139800050331     c                   if        wfaca = writi  or  wfaca = wripl
139900020131    1c                             or $Franch = *on
140000080123     c                             or wSAVcch = WchBM
140100990430      * Aggiorno testata C.A. importi solo per ritorno al pagamento
140200990430     c                   EXsr      UPDdcl
140300990430     c                   endif
140400050805      *
140500131011      * Ripulisco il flag per richiesta rimborso ad A.I.G. su FNDET00F
140600050805      *   (DETFTR) se quest'ultimo è già impostato a "P" o "N"
140700050805     c                   exsr      UpdDET
140800981014      *
140900981014      * Scrivo fasi C.A.
141000981014     c                   EXSR      WRTDCF
141100010926      *
141200050218      * Se sono decorsi i gg. della tab. 5A dalla data chiusura
141300090417      * aggiorno il file FnRIG00R - rigenerazione bolle x P.O.
141400010926    2c                   if        $over4m = *on
141500010926     c                   EXSR      WRTfirig
141600010926    2c                   endif
141700020128      *
141800050401      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
141900050401     c                   if        wfaca = writi  or  wfaca = wripl
142000050401      * scrivo note estremi aseegno rifiutato
142100050401     c                   EXsr      WRTDCS
142200050401     c                   endif
142300981014      *
142400981014     c                   ENDSR
142500990430      *****************************************************************
142600990430      * AGGIORNO FNDCL Testata C.A. Importi
142700990430      *****************************************************************
142800990430     C     UPDDCL        BEGSR
142900990430     c*
143000990430      * pulisco i campi relativi al pagamento ma lascio gli importi
143100020201      * => NON IN CASO DI FRANCHIGIA
143200990430      *
143300990430     c* apro e aggancio  il file liquidazione ca
143400010508      *
143500010508     C                   if        not %OPEN(Fndcl01L)
143600990430     c                   open      fndcl01l
143700010508     c                   endif
143800010508      *
143900990430     c     keynca        chain     fndcl01l                           30
144000010508      * pulisco  i dati relativi il pagamento (ABI/CAB, numero assegno, dta pagamento)
144100010508     c                   clear                   dclfic
144200990430     c                   clear                   dclabi
144300990430     c                   clear                   dclcab
144400990430     c                   clear                   dclasn
144500990430     c                   clear                   dcldea
144600020128      * pulisco le date relative alla stampa ed accettazione progetto di liquidazione
144700080114     c                   IF        $Franch = *on
144800020128     c                   clear                   DCLdds
144900020128     c                   clear                   DCLdpl
145000080114     c                   clear                   DCLipl
145100080114     c                   clear                   DCLvpl
145200080114     c                   ENDIF
145300080123      *
145400080123      * Ripulisco l'importo eventualmente liquidato
145500080123     c                   IF        wSAVcch = WchBM
145600080123     c                   clear                   DCLipl
145700080123     c                   clear                   DCLvpl
145800080123     c                   clear                   DCLvlp
145900080123     c                   ENDIF
146000080123      *
146100990430     c                   clear                   dclftr
146200080123     c  n30              update    fndcl000
146300010508      *
146400010508     C                   if        %OPEN(Fndcl01L)
146500010508     c                   close     fndcl01l
146600010508     c                   endif
146700150702
146800150702      /free
146900150702       //?Cancella rcd 'P' in FNDKA, beneficiario
147000150702         chain (kaac:kfil:knca:'P') FNDKA01L;
147100150702         IF  %found(FNDKA01L);
147200150702           delete FNDKA000;
147300150702         ENDIF;
147400150702      /end-free
147500990430     c*
147600990430     c                   endsr
147700050805
147800050805      **********************************************************************
147900050805      *  AGGIORNAMENTO TESTATA EVENTI
148000050805      **********************************************************************
148100050805     C     UpdDET        BEGSR
148200050805      *
148300131011      * Ripulisco il flag di RIMBORSO A.I.G. su FNDET00F
148400131011      * se  "P" = Ricezione totale del rimborso da A.I.G.
148500050805      *  o  "N" = Evento non fortuito che ha le CA pagate ma < 10000€
148600050805      *
148700050805     c     K02DET01      chain     FNDET000
148800050805      *
148900050805if  1c                   if        %found(FNDET01L)
149000050805     c                             and (DETftr = 'P'
149100050805     c                              or  DETftr = 'N')
149200050805     c                   clear                   DETftr
149300050805     c                   UPDATE    FNDET000
149400050805e   1c                   endif
149500050805      *
149600050805     C                   ENDSR
149700050126      *****************************************************************
149800050126      * AGGIORNO FNDCL data accettazione P.L.
149900050126      *****************************************************************
150000050126     C     UPDDCL1       BEGSR
150100050126      *
150200050126     c* apro e aggancio  il file liquidazione ca
150300050126      *
150400050126     C                   if        not %OPEN(Fndcl01L)
150500050126     c                   open      fndcl01l
150600050126     c                   endif
150700050126      *
150800050126     c     keynca        chain     fndcl01l                           30
150900080115      *
151000080115      * Imposto l'importo liquidato
151100080115     c                   IF        NOT  *in08
151200080115     c                             and  V1Ccch = WchBM
151300080123      *
151400080115     c                   if        *in30
151500080115     c                   clear                   FNDCL000
151600080115     c                   eval      DCLaac = Kaac
151700080115     c                   eval      DCLfil = Kfil
151800080115     c                   eval      DCLnca = Knca
151900080115     c                   endif
152000080123     c                   if        V2Cipl > *zeros
152100080115     c                   eval      DCLipl = V2Cipl
152200080115     c                   eval      DCLvpl = V2Cvpl
152300080115     c                   else
152400080115     c                   clear                   DCLipl
152500080115     c                   clear                   DCLvpl
152600080115     c                   endif
152700080123     c                   eval      DCLvlp = DateU
152800080123      *
152900080123     c                   ELSE
153000080123      *
153100050126     c                   z-add     dateu         dcldpl
153200080123      *
153300080123     c                   ENDIF
153400080123      *
153500050126     c                   clear                   dclftr
153600080115      *
153700080115     c                   select
153800080115     c                   when      NOT *in30
153900080115     c                   UPDATE    FNDCL000
154000080115     c                   when      *in30  and  DCLipl > *zeros
154100080115     c                   WRITE     FNDCL000
154200080115     c                   endsl
154300050126      *
154400050126     C                   if        %OPEN(Fndcl01L)
154500050126     c                   close     fndcl01l
154600050126     c                   endif
154700050126     c*
154800050126     c                   endsr
154900981014      *****************************************************************
155000981014      * AGGIORNO FNDCD Dettaglio C.A.
155100981014      *****************************************************************
155200981014     C     UPDDCD        BEGSR
155300981014     c*
155400981014     c     keynca        setll     fndcd01l
155500981014     c     keynca        reade     fndcd01l                               31
155600981014     c*
155700981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
155800981014     c                   dow       *in31 = *off
155900981014     c                   eval      DCddch = dctdch
156000981014     c                   eval      DCdcch = dctcch
156100981222     c                   eval      DCdftr = dctft1
156200981014     c                   update    fndcd000
156300981014     c*
156400981014     c     keynca        reade     fndcd01l                               31
156500981014     c*
156600981014     c                   enddo
156700981014     c*
156800981014     c                   endsr
156900980923      *****************************************************************
157000980923      * AGGIORNO FNDCF FASI C.A.
157100980923      *****************************************************************
157200980923     C     WRTDCF        BEGSR
157300980923      *
157400990226     c                   clear                   fndcf000
157500020201     c                   clear                   DDCF01
157600980923     C                   z-add     DCTaaC        DCFaac
157700980923     C                   z-add     DCTfil        DCFfil
157800980923     C                   z-add     DCTnca        DCFnca
157900981014     c                   z-add     dctfca        DCFfca
158000980923     c                   z-add     dateu         DCFdfc
158100980923     c                   z-add     wora          DCFhfc
158200980923     c                   z-add     V1Cfgs        DCFfev
158300981026     c                   movel     dctptr        DCFptr
158400980923     c                   movel     knmus         DCFpru
158500020201     c                   movel     §DCTpfra      §DCFpfra
158600020201     c                   movel     DDCF01        DCFlet
158700980923      *
158800980923     c                   WRITE     FNDCF000
158900980923      *
159000980923     c                   ENDSR
159100981105      *****************************************************************
159200981105      * AGGIORNO ARCHIVI   RIAPERTURA C.A. PER RIFIUTO TRANSAZIONE
159300981105      *****************************************************************
159400981105     C     SUB_RIFIU     BEGSR
159500990312      * disalloco il file fndct
159600990312     c                   unlock    fndct01l
159700981105      * Richiamo il pgm rifiuto transazione
159800981105      *
159900981105     c                   clear                   FIDN00DS
160000981105      *
160100981105     c                   movel     'G'           I00mod
160200981105     c                   movel     wrifi         I00fca
160300981105     c                   z-add     dctgfc        I00fgs
160400981105     c                   z-add     dctaac        I00aac
160500981105     c                   z-add     dctfil        I00fil
160600981105     c                   z-add     dctnca        I00nca
160700981105     c                   z-add     dctaas        I00aas
160800981105     c                   z-add     dctlnp        I00lnp
160900981105     c                   z-add     dctnrs        I00nrs
161000981105     c                   z-add     dctnsp        I00nsp
161100981105     c                   z-add     dctlna        I00lna
161200981105     c                   movel     dcttad        I00tad
161300981230     c                   if        dctgfc = W§dctlnpc
161400981105     c                   movel     'P'           I00tpb
161500981105     c                   endif
161600981105     c                   if        dctgfc = dctlna
161700981105     c                   movel     'A'           I00tpb
161800981105     c                   endif
161900981105     c                   if        dctgfc = 046
162000981105     c                   movel     'S'           I00tpb
162100981105     c                   endif
162200981105      *
162300981105     c                   movel     FIDN00DS      kpjbu
162400981105      *
162500981105     c                   call      'FIDN23R'
162600981105     c                   parm                    kpjba
162700990312     c                   movel     kpjbu         fidn00ds
162800990312     c*
162900990312      *
163000990312     C     Keynca        chain     FNDCT000                           31
163100990312      *
163200990312      * se non viene dato KL/KC  in FIDN23 aggiorno altrimenti riemetto la videata
163300990312     c                   if        o00f12 = ' ' and o00f03 = ' '
163400990312      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
163500990312     c                   z-add     *zeros        DCTdch
163600990312     c                   clear                   DCTcch
163700990312     c                   clear                   DCTft1
163800990312     c                   clear                   DCTft2
163900990312      *
164000990312      * Aggiorno testata C.A.
164100990312     c                   EXCEPT    UPDdct
164200990312      * Aggiorno l dettaglio C.A.
164300990312     c                   EXSR      UPDDCD
164400990312      *
164500990312     c                   endif
164600981105      *
164700981105     c                   ENDSR
164800050331      *****************************************************************
164900050331      *  SCRITTURA NOTE AGGIUTIVE
165000050331      *****************************************************************
165100050331     C     WRTDCS        BEGSR
165200050331      *
165300050331      * cerco l'ultimo progressivo note della fase in corso
165400050331      * - campi strandard
165500050331     c                   eval      Ktpd = 'C'
165600050331     c                   eval      aac = kaac
165700050331     c                   eval      fil = kfil
165800050331     c                   eval      nca = knca
165900050331     c                   eval      Knkt = dsnumca
166000050331     c                   eval      Kstd = ' '
166100050331     c                   eval      Kdim = dateu
166200050401     c                   eval      Khim = wora
166300050331      * - dati relativi alla CA in manutenzione
166400050331     c                   movel(p)  900           Knks
166500050331     c                   eval      Ktrc = 'N'
166600050331      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
166700050331     c     Kdcs          setgt     FNDCS01L
166800050331     c     Kdcs          readpe    FNDCS01L
166900050331      *
167000050331     c                   clear                   DCSatb
167100050331     c                   eval      DCStpd = Ktpd
167200050331     c                   eval      DCSnkt = Knkt
167300050331     c                   eval      DCSstd = Kstd
167400050331     c                   eval      DCSdim = Kdim
167500050331     c                   eval      DCShim = Khim
167600050331     c                   eval      DCSnks = Knks
167700050331     c                   eval      DCStrc = Ktrc
167800050331     c                   IF        %eof(FNDCS01L)
167900050331     c                   eval      DCSpno = 1
168000050331     c                   else
168100050331     c                   eval      DCSpno = DCSpno + 1
168200050331     c                   ENDIF
168300050401     c                   eval      DCSnot ='Il Cliente ha rifiutato l''assegno '
168400050331     c                   eval      DCSpru = KNMUS
168500050331     c                   eval      DCSpos = v1cfgs
168600050331     c                   clear                   DCSstn
168700050331     c                   clear                   DCSft1
168800050331     c                   clear                   DCSdt1
168900050331      *
169000050331     c                   write     FNDCS000
169100050401      * 2° riga
169200050401     c                   eval      DCSpno = DCSpno + 1
169300050401     c                   clear                   dcsnot
169400050401     c
169500050401     c                   eval      DCSnot = 'di traenza n°' + %trim(v1casn) +
169600050401     c                                      ' emesso il '
169700050401
169800050401     c                   write     FNDCS000
169900050401      * 3° riga
170000050401     c                   eval      DCSpno = DCSpno + 1
170100050401     c                   clear                   dcsnot
170200050401     c
170300050401     c                   eval      DCSnot = %editw(v1cdea:'  /  /    ') + ' di '
170400050401     c                             + %trim(%editc(v1cipl:'3')) + ' ' + v1cvpl
170500050331      *
170600050401
170700050401     c                   write     FNDCS000
170800050331     C                   ENDSR
170900120508      *****************************************************************
171000120508      *   REPERISCO NUMERO PRATICA ASSICURATIVA
171100120508      *****************************************************************
171200120508     C     NUMPRN        BEGSR
171300120508
171400120508      * verifico se CA legata ad evento
171500120508      * se legata verifico se nel file delle denucnce c'è il suo numero pratica
171600120508     c                   If        dctnev > 0
171700120508     c     K02DET01      chain     fndet01l
171800120508      * non trovo sul file denuncia vado via
171900120508     c                   If        not %found(fndet01l)
172000120508     c                   leavesr
172100120508     c                   endif
172200120508      * c'è il numero pratica imposto i campi e poi vado via
172300120508     c                   If        detprn > 0
172400120508     c                   unlock    fndet01l
172500120508     c                   eval      dctdit  =   DETdit
172600120508     c                   eval      dctpra  =   DETpra
172700120508     c                   eval      dctprn  =   DETprn
172800120508     c                   leavesr
172900120508     c                   endif
173000120508      * non c'è il numero pratica lo recupero
173100120508     c                   Clear                   FIDN20DS
173200120508     c                   Movel     'N'           I20mod
173300120508     c                   Z-add     DETaae        I20pra
173400120508     c                   Movel     DETdit        I20dit
173500120508     c                   movel(P)  FIDN20DS      KPJBU
173600120508     c                   CALL      'FIDN20R'
173700120508     c                   PARM                    KPJBA
173800120508     c                   movel     KPJBU         FIDN20DS
173900120508      * torna errore vado via
174000120508     c                   IF        O20err = 'E'
174100120508     c                   leavesr
174200120508     c                   ENDIF
174300120508      * c'è il numero pratica imposto i campi aggiorno il file denuncia
174400120508     c                   movel     I20dit        DETdit
174500120508     c                   z-add     I20pra        DETpra
174600120508     c                   z-add     O20prn        DETprn
174700120508     c                   update    fndet000
174800120508      * imposto i campi per la testata CA e vado via
174900120508     c                   eval      dctdit  =   DETdit
175000120508     c                   eval      dctpra  =   DETpra
175100120508     c                   eval      dctprn  =   DETprn
175200120508     c                   leavesr
175300120508     c                   endif
175400120508
175500120508      * La CA non è legata ad evento
175600120508      * recupero un numero pratica
175700120508     C                   clear                   FIDN20DS
175800120508     C                   movel     'N'           I20mod
175900120508     c                   z-add     DCTaas        I20pra
176000120508     c                   movel     'BAR'         I20dit
176100120508      * se la spedizione ha valore da assicurare esposto in bolla o
176200120508      *   assicurazione per conto: verifico se il P.O. di arrivo ha
176300120508      *   FEDEX come network
176400120508     c                   if        TASias > 0 and TASfcm <> 'F'
176500120508     c     DCTlna        Chain     AZORG
176600120508if  1c                   If            %found(AZORG01L)
176700120508     c                             and ORGfva = *blanks
176800120508     c                   Movel     ORGde3        OG143
176900120508if  2c                   If        §OGntw = 'FED'
177000120508     c                   Movel     §OGntw        I20dit
177100120508e   2c                   Endif
177200120508e   1c                   Endif
177300120508     c                   endif
177400120508     C                   movel(P)  FIDN20DS      KPJBU
177500120508     c                   CALL      'FIDN20R'
177600120508     c                   PARM                    KPJBA
177700120508     C                   movel     KPJBU         FIDN20DS
177800120508      * torna errore vado via
177900120508     c                   IF        O20err = 'E'
178000120508     c                   eval      *in28 = *ON
178100120508     c                   leavesr
178200120508     c                   ENDIF
178300120508      * imposto i campi per la testata CA
178400120508     c                   movel     I20dit        DCTdit
178500120508     c                   z-add     I20pra        DCTpra
178600120508     c                   z-add     O20prn        DCTprn
178700120508      *
178800120508     c                   ENDSR
178900980910      *****************************************************************
179000980527      * ROUTINE INIZIALE
179100980527      *****************************************************************
179200980527     C     *INZSR        BEGSR
179300980615      *
179400980521     C     *ENTRY        PLIST
179500980521     C                   PARM                    KPJBA
179600981014     c                   movel     kpjbu         fidn22ds
179700981014     c* se numero CA diversa da zeros  vuol dire che il programma è stato richiamato
179800981015     c                   if        i22tri = 'R'
179900981014     c                   eval      v1cfil = i22fil
180000981014     c                   eval      v1caac = i22aac
180100981014     c                   eval      v1cnca = i22nca
180200981021     c                   eval      v1ccch = i22cch
180300981202     c                   eval      v1cfgs = i22fgs
180400981021     c* decodifico causale chiusura se c'è
180500981021     c                   if        v1ccch <> '  '
180600981021     c                   clear                   dsbs02
180700981021     C                   MOVEL     'C'           t02mod
180800981021     C                   MOVEL     knsif         t02sif
180900981021     C                   MOVEL     'CCH'         t02cod
181000981021     C                   MOVEL(P)  v1ccch        t02ke1
181100981021      *
181200981021     C                   CALL      'TIBS02R'
181300981021     C                   PARM                    KPJBA
181400981021     C                   PARM                    dsbs02
181500981021      *
181600981021    4C                   if        t02err = *BLANKS
181700981021     C                   MOVEL     T02UNI        v1dcch
181800981021     c                   endif
181900990122     c*
182000990122     c                   else
182100010508      *
182200010508     C                   if        not %OPEN(Fidn22d)
182300990122     c                   open      fidn22d
182400010508     c                   endif
182500981021      *
182600981021     c                   endif
182700981021      *
182800981021     c* proteggo i campi del cambio p.o gestione e non abilito il CMD3
182900981021     c                   seton                                        060710
183000990122     c*
183100990122     c                   else
183200010508     C                   if        not %OPEN(Fidn22d)
183300010508     c                   open      fidn22d
183400010508     c                   endif
183500981014     c                   endif
183600980907      *
183700050929      * Reperisco dati job
183800050929     c                   exsr      DatiJob
183900050929     c                   eval      TBLkut = 1
184000020128      *
184100020128      * Reperisco la moneta corrente
184200020128     C                   clear                   DSBS02
184300020128     C                   movel     'L'           T02tla
184400020128     C                   movel     'C'           T02mod
184500020128     C                   movel     KNSIF         T02sif
184600020128     C                   movel     'GED'         T02cod
184700020128     C                   movel     'DANNI'       T02ke1
184800020128     C                   call      'TIBS02R'
184900020128     C                   parm                    KPJBA
185000020128     C                   parm                    DSBS02
185100020128     C                   If        T02err = *blanks
185200020128     C                   movel     T02UNI        DGEDDN
185300020128     C                   else
185400020128     C                   clear                   DGEDDN
185500020128     C                   Endif
185600020128      *
185700020128      * Reperisco tab.STD
185800020128     C                   clear                   DSBS02
185900020129     C                   movel     'L'           T02tla
186000020128     C                   movel     'C'           T02mod
186100020128     C                   movel     KNSIF         T02sif
186200020128     C                   movel     'STD'         T02cod
186300020128     C                   movel     '1'           T02ke1
186400020128     C                   movel     §gedDBA       T02ke2
186500020129     C                   call      'TIBS02R'
186600020129     C                   parm                    KPJBA
186700020129     C                   parm                    DSBS02
186800020128     C                   If        T02err = *blanks
186900020128     C                   movel     T02uni        DSTD
187000020128     C                   else
187100020128     C                   clear                   DSTD
187200020128     C                   Endif
187300980907
187400000228      * IMPOSTO IL P.O. IN GESTIONE se = a zeros
187500000228     c                   if        v1cfgs = *zeros
187600050929    2C                   IF           DUTlpo = '2'
187700050929     C                             or DUTlpo = *blanks
187800050929     C                   movel     DUTpou        V1CFGS
187900980521     C                   eval      *in06 = *on
188000980521   X2C                   ELSE
188100980521     C                   movel     SIMFEL        V1CFGS
188200980521    2C                   ENDIF
188300070404     c                   endif
188400981001      *
188500981001      * Se sede non visualizzo  "P.O. IN GESTIONE"
188600981001     C                   IF        V1Cfgs = *zeros
188700981001     C                   eval      *in06 = *on
188800981001     C                   eval      *in07 = *on
188900981001     C                   z-add     46            V1CFGS
189000070404     c**!!!              ELSE
189100070404     c                   endif
189200070404      * Se non è sede carico £6 è altri dati
189300070404     c                   if        v1cfgs <> 46
189400070404     c                   clear                   og148
189500981012     C     V1CFGS        CHAIN     AZORG                              31
189600980521     C  N31              MOVEL     orgDES        V1DFGS
189700070404     c  n31              eval      og148 = orgde8
189800070404      * controllo se Filiale gestione è abilitata a telefonate GEO
189900070404     c                   eval      wfilgeo = (§oggeot = 'S')
190000981014     C                   exsr      CARL6
190100070404    2C**!!!              ENDIF
190200000228     c                   endif
190300981007      *
190400981007      * Imposto il TITOLO
190500981014      * chiusura
190600981014     c                   if        i22mod = 'C'
190700981014     c                   movel     TIT_chca      VTCtit
190800981130     c* valorizzo la fase di chiusura
190900981130     c                   eval      wfaca = wchca
191000981014     c                   endif
191100981014      * riapertura
191200981014     c                   if        i22mod = 'R'
191300981014     c                   movel     TIT_raca      vtctit
191400981130     c* pulisco la fase
191500981130     c                   clear                   wfaca
191600981014     c                   seton                                        08
191700981014     c                   endif
191800980721      * reperisco data e ora
191900980721     C                   TIME                    W0140
192000980521      * UDATE IN GGMMAAAA
192100980521     C                   MOVE      W0140         WDTGIO
192200980923     C                   MOVEL     W0140         WORA
192300980521      * UDATE IN AAAAMMGG
192400980521     C                   Z-ADD     WDTGIO        G02DAT
192500980521     C                   MOVEL     *BLANK        G02ERR
192600980521     C                   CALL      'XSRDA8'
192700980521     C                   PARM                    WLBDAT
192800980521     C                   MOVEL     G02INV        DATEU
192900980923
193000000117      *    Imposto anno C.A.  se non sono stato richiamato
193100000117     c                   if        i22tri <> 'R'
193200981013     c                   move      wdtgio        V1Caac
193300000117     c                   endif
193400050218
193500050218      * Aggancio tabella 5A rcd 2
193600050218     c                   Clear                   ds5a2
193700050218     c                   Eval      tblcod = '5A'
193800050218     c                   Eval      tblkey = '2       '
193900050218     c     ktab          Chain     Tabel00f
194000050218     c                   If        %Found(Tabel00f)
194100050218     c                   Eval      ds5a2 = TblUni
194200050218     c                   EndIf
194300980721
194400981014     C     Keynca        klist
194500981013     C                   KFLD                    kaac
194600981013     C                   KFLD                    kfil
194700981013     C                   KFLD                    knca
194800050805      * File Eventi
194900050805     c     K02DET01      klist
195000050805     c                   kfld                    DCTaae
195100050805     c                   kfld                    DCTnev
195200981016      * File giacenza
195300981016     c     kgiac         klist
195400040722     c                   kfld                    giaaas
195500040722     c                   kfld                    gialnp
195600040722     c                   kfld                    gianrs
195700040722     c                   kfld                    giansp
195800981016     c                   kfld                    Wfrg
195900981016      * File Bolle
196000981016     c     karb          klist
196100981016     c                   kfld                    dctaas
196200981016     c                   kfld                    dctlnp
196300981016     c                   kfld                    dctnrs
196400981016     c                   kfld                    dctnsp
196500010926      * file TITAS30C
196600010926     c     K04TAS30C     klist
196700010926     c                   kfld                    DCTaas
196800010926     c                   kfld                    DCTlnp
196900010926     c                   kfld                    DCTnrs
197000010926     c                   kfld                    DCTnsp
197100050218
197200050218     c     ktab          klist
197300050929     c                   kfld                    TBLkut
197400050218     c                   kfld                    tblcod
197500050218     c                   kfld                    tblkey
197600050331      * file FNDCS00F
197700050331     C     KDCS          KLIST
197800050331     C                   KFLD                    KTPD
197900050331     C                   KFLD                    KNKT
198000050331     C                   KFLD                    KSTD
198100050331     C                   KFLD                    KDIM
198200050331     C                   KFLD                    KHIM
198300050331     C                   KFLD                    KNKS
198400050331     C                   KFLD                    KTRC
198500980915
198600980521     C                   ENDSR
198700050929      *
198800050929      *---------------------------------------------------------------*
198900050929      * Reperimento Dati del job (Utente/Operativi)                   *
199000050929      *---------------------------------------------------------------*
199100050929     c     DatiJob       BEGSR
199200050929      *
199300050929     c     *dtaara       define    §azute        azuteds
199400050929     c     *dtaara       define    §datiute      ddatiute
199500050929      *
199600050929     c                   in(E)     *dtaara
199700050929     c                   IF        %ERROR or RSUT = *blanks
199800050929     c                   clear                   Tibs34Ds
199900050929     c                   call      'TIBS34R'
200000050929     c                   parm                    Tibs34Ds
200100050929     c                   in        *dtaara
200200050929     c                   ENDIF
200300050929      *
200400050929     c                   ENDSR
200500010926
200600010926      *****************************************************************
200700011008      * AGGANCIO FILE TITAS30C (PER CONTROLLO DATA BOLLA, MA NON SOLO)
200800010926      *****************************************************************
200900011008     c     ChaTITAS      BEGSR
201000010926      *
201100010926     c                   if        not %OPEN(TITAS30C)
201200010926     c                   open      TITAS30C
201300010926     c                   endif
201400010926      *
201500010926     c     K04TAS30C     chain     TITAS30C
201600011008     c                   if        not %found
201700011008     c                   clear                   TASaas
201800011008     c                   clear                   TASlnp
201900011008     c                   clear                   TASnrs
202000011008     c                   clear                   TASnsp
202100011008     c                   clear                   TASlna
202200010926     c                   endif
202300010926      *
202400010926     c                   ENDSR
202500010926
202600010926      *****************************************************************
202700090417      * AGGIORNAMENTO FILE FnRIG00f
202800010926      *****************************************************************
202900010926     c     WRTfirig      BEGSR
203000010926      *
203100090417     c                   if        not %OPEN(FnRIG00F)
203200090417     c                   open      FnRIG00F
203300010926     c                   endif
203400010926      *
203500090417     c                   clear                   FnRIG000
203600010926     c                   eval      RIGaas = TASaas
203700010926     c                   eval      RIGlnp = TASlnp
203800010926     c                   eval      RIGnrs = TASnrs
203900010926     c                   eval      RIGnsp = TASnsp
204000010926     c                   eval      RIGlna = TASlna
204100090417     c                   eval      RIGcau = 'D'
204200010926     c                   eval      RIGtra = 'E'
204300090417     c                   eval      RIGfil = V1Cfgs
204400010926     c                   eval      RIGpru = KNMUS
204500090417     c                   eval      RIGdata = Dateu
204600090417     c                   eval      RIGora = wora
204700010926      *
204800090417     c                   write     FnRIG000
204900010926      *
205000010926      * ...e rigenero la bolla legata alla C.A. riaperta
205100010926     c                   clear                   KPJBU
205200010926     c                   eval      kcoaz = 'BS21'
205300010926     c                   call      'BCH10'
205400010926     c                   parm                    kpjba
205500020128      *
205600010926     c                   ENDSR
205700080114      *
205800080114      *****************************************************************
205900080114      * Gestione videata D02
206000080114      *****************************************************************
206100080114     c     GesD02        BEGSR
206200080114      *
206300080114      * Inizializzazione videata
206400080114     c                   if        $InzD2 = *on
206500080114     c                   exsr      InzD02
206600080114     c                   eval      $InzD2 = *off
206700080114     c                   endif
206800080114      * Emissione videata
206900080114     c                   if        NOT *in90
207000080114     c                   write     FI22T01
207100080114     c                   endif
207200080114     c                   exfmt     FI22D02
207300080114     c                   setoff                                       28  90
207400080114     c                   clear                   V1Cmsg
207500080114      *
207600080114     c                   SELECT
207700080114      * F3=Fine
207800080114     c                   WHEN      *inKC
207900080114     c                   reset                   $Video
208000080114     c                   eval      O22f03 = 'S'
208100080114      * F12=Ritorno
208200080114     c                   WHEN      *inKL
208300080114     c                   reset                   $Video
208400080114      * F10=Note C.A.
208500080114     c                   WHEN      *inKJ
208600080114     c                   exsr      Sub_F10
208700080114      * F14=Interrogazione C.A.
208800080114     c                   WHEN      *inKN
208900080114     c                   exsr      Sub_F14
209000080114      * Invio / F6
209100080114     c                   OTHER
209200080114     c                   exsr      CtrD02
209300080115     c                   if        NOT *in90  and  *inKF
209400080115      * - Conferma delle eventuali note precedentemente inserite
209500080115     c                   clear                   dsDN10
209600080115     c                   eval      I10dta = WdataCA
209700080115     c                   eval      I10flm = 'C'
209800080115     c                   eval      I10tpd = 'C'
209900080115     c                   movel(p)  dsNumCA       I10nkt
210000080115     c                   movel(p)  WchCA         I10nks
210100080115     c                   eval      I10trc = 'N'
210200080115     c                   movel(p)  dsDN10        kpjbu
210300080115     c                   call      'FIDN10R'
210400080115     c                   parm                    kpjba
210500080115     c                   movel     kpjbu         dsDN10
210600080115      * - Note fase ca
210700080115     c                   exsr      Sub_F10
210800080115      * - Uscita
210900080115     c                   clear                   $Video
211000080115     c                   endif
211100080115     c                   ENDSL
211200080114      *
211300080114     c                   ENDSR
211400080114      *
211500080114      *****************************************************************
211600080114      * Inizializzazione videata D02
211700080114      *****************************************************************
211800080114     c     InzD02        BEGSR
211900080114      *
212000080114     c                   movea     *in           Sav_In
212100080114     c                   clear                   FI22D02
212200080114     c                   movea     Sav_In        *in
212300080114      *
212400080114      * - Riporto i dati da D01
212500080114     c                   eval      V2Cfgs = V1Cfgs
212600080114     c                   eval      V2Dfgs = V1Dfgs
212700080114     c                   eval      V2Cfil = V1Cfil
212800080114     c                   eval      V2Cnca = V1Cnca
212900080114     c                   eval      V2Caac = V1Caac
213000080114     c                   eval      V2Ccch = V1Ccch
213100080114     c                   eval      V2Dcch = V1Dcch
213200080114      *
213300080114      * - Importo massimo risarcibile
213400080114     c                   eval      V2Cipv = DCTipv
213500080114     c                   eval      V2Cvpv = DCTvpv
213600080123      * - Tipo Calcolo
213700080123     c                   select
213800080123     c                   when      DCTfpv = 'M'
213900080123     c                   eval      V2Dfpv = 'Inserito manualmente'
214000080123     c                   when      DCTfpv = 'C'
214100080123     c                   eval      V2Dfpv = 'Calcolato'
214200080123     c                   endsl
214300080114      *
214400080114      * Reperimento "importo richiesto" ed "importo liquidato"
214500080114if  1c                   if        NOT %open(FNDCL01L)
214600080114     c                   open      FNDCL01L
214700080114e   1c                   endif
214800080114     c     KEYnca        chain(n)  FNDCL000
214900080114if  1c                   IF        %found(FNDCL01L)
215000080114      * - Importo richiesto
215100080114     c                   eval      V2Cipr = DCLipr
215200080114     c                   eval      V2Cvpr = DCLvpr
215300080114      * - Importo liquidato
215400080114     c                   eval      V2Cipl = DClipl
215500080115     c                   eval      V2Cvpl = DCLvpl
215600080114e   1c                   ELSE
215700080114     c                   clear                   V2Cipr
215800080114     c                   clear                   V2Cvpr
215900080114     c                   clear                   V2Cipl
216000080114e   1c                   ENDIF
216100080115     c                   if        V2Cvpl = *blanks
216200080115     c                   eval      V2Cvpl = §gedDBA
216300080115     c                   endif
216400080114if  1c                   if        %open(FNDCL01L)
216500080114     c                   close     FNDCL01L
216600080114e   1c                   endif
216700080114      *
216800080114     c                   ENDSR
216900080114      *
217000080114      *****************************************************************
217100080114      * Controllo dati videata D02
217200080114      *****************************************************************
217300080114     c     CtrD02        BEGSR
217400080114      *
217500080114     c                   movea     *zeros        *in(40)
217600080115      *
217700080115     c                   if        V2Cipl < *zeros
217800080115     c                   seton                                        284390
217900080115     c                   eval      V1Cmsg = Msg(22)
218000080115     c                   leavesr
218100080115     c                   endif
218200080114      *
218300080114     c                   ENDSR
218400080114
218500980521      *---------------------------------------------------------------------------------------------
218600981007     oFNDCT000  E            UPDDCT
218700981013     O                       DCTdch
218800981013     O                       DCTcch
218900981013     O                       DCTfca
219000080701     O                       DCTgfc
219100020226     O                       DCTpgd
219200120507     O                       DCTksc
219300120508     o                       DCTDIT
219400120508     o                       DCTPRA
219500120508     o                       DCTPRN
219600990430     O                       DCTflo
219700981222     O                       DCTft1
219800981222     O                       DCTft2
219900050408     O                       DCTdt2
220000980923      *---------------------------------------------------------------------------------------------
220100980521** MSG  Lungh. 78                                                            *
220200981013Il numero C.A. immesso non esiste
220300981014Il valore immesso nel campo non è valido                                       2
220400981014Causale di chiusura errata                                                     3
220500080114C.A. in gestione da altra filiale                                              4
220600981014C.A. già chiusa                                                                5
220700981014C.A. non chiusa impossibile riaprirla                                          6
220800981021Impossibile riaprire C.A. : pratica giacenza aperta                            7
220900010926La C.A. non si può più riaprire perchè la bolla è antecedente il 01/09/1999    8
221000981021Impossibile riaprire C.A. : spedizione con blocco                              9
221100990311Non si può usare questa causale per chiusure non automatiche                  10
221200990311C.A. in fase di contabilizzazione impossibile riaprirla                       11
221300990311C.A. in fase di pagamento per Pratica Assicurativa impossibile riaprirla      12
221400990311La C.A. non si può più riaprire per mancanza dettaglio colli                  13
221500050218La C.A. non si può più riaprire perchè decorsi xxx gg. da data chiusura       14
221600050126Non si può usare questa causale se non stampato progetto liquidazione         15
221700050126Causale di chiusura che può usare solo la sede                                16
221800050331Utente non autorizzato ad aprire questa C.A.                                  17
221900050512Causale Chiusura utilizzabile per spedizioni di data &PRE/SUC      17/04/2005 18
222000131011La C.A. non si può riaprire. Avviate richieste di recupero ad A.I.G.          19
222100131011Causale di Pagamento utilizzabile solo per C.A. NON affidata ad A.I.G.        20
222200050929Causale di chiusura utilizzabile solo da utenti di                            21
222300080115L'importo liquidato NON può essere negativo                                   22
