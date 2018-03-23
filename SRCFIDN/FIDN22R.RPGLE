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
017000980923      *
017100980923     D WLBDAT          DS                  INZ
017200980923     D  G02DAT                 1      8  0
017300980923     D  G02INV                 9     16  0
017400980923     D  G02ERR                17     17
017500980923     D  G02TGI                18     22  0
017600980915      *----------------------------------
017700050929      * - Parametri x Controllo profilo utenti
017800050929     d TIBS34DS      e ds                  inz
017900050929      * - Ds di riferimento al file esterno AZUTE00F
018000050929     d AZUTEds       e ds                  extname(AZUTE00F)
018100050929      * - Ds per dati organigramma
018200050929     d DDatiUte      e ds
018300050929      *
018400120508     D FIDN20DS      E DS
018500981015     D Fidn22DS      E DS
018600981016     D Fidn00DS      E DS
018700120507     d FIDN40DS      e ds
018800981014     D FNLV50DS      E DS
018900990311     D DCCH          E DS
019000050331     D  UTE                   60    109       dim(5)                            utenti autorizzati
019100981230     D DDCT01        E DS
019200020201     D DDCF01        E DS
019300981014     D DSbs02        E DS                  EXTNAME(tibs02ds)
019400981014     D DSDN10        E DS                  EXTNAME(FIDN10DS)
019500981014     D TRUL06DS      E DS
019600981014     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
019700020128     D DGEDDN        E DS                  INZ
019800020128     D DSTD          E DS                  INZ
019900050218     d ds5a2         e ds
020000120508     d OG143         e ds                  inz
020100070404     d og148         e ds
020200070404     d dslr02        e ds
020300980521      *
020400980521     D KPJBA         E DS
020500980521      *
020600980521     D                SDS
020700980521     D  VTCPGM                 1     10
020800981015      *
020900981015     d dsnumca         ds
021000981015     d  aac                    1      4  0
021100981015     d  fil                    5      7  0
021200981015     d  nca                    8     14  0
021300050401
021400050401     D Wdataeur        S               D   DATFMT(*eur)
021500050331
021600050331     D ktpd            s                   LIKE(DCStpd)
021700050331     D knkt            s                   LIKE(DCSnkt)
021800050331     D kstd            s                   LIKE(DCSstd)
021900050331     D knks            s                   LIKE(DCSnks)
022000050331     D ktrc            s                   LIKE(DCStrc)
022100050331     D kdim            s                   LIKE(DCSdim)
022200050331     D khim            s                   LIKE(DCShim)
022300080114      *
022400080114     d Sav_In          s             99    inz(*zero)
022500980623
022600990122     c*---------------------------------------------------------------------------------------------
022700990122     c* se la causale di chiusura è valorizzata non emetto videate
022800990122     c*---------------------------------------------------------------------------------------------
022900990122     c* Pgm richiamato di chiusura con la causale di chiusura passante
023000990122     c                   if        i22tri = 'R' and i22mod = 'C' and
023100990122     c                             I22cch <> '  '
023200990122     c* valorizzo la chiave CA
023300990122     C                   z-add     V1Cfil        Kfil
023400990122     C                   z-add     V1Caac        Kaac
023500990122     C                   z-add     V1Cnca        Knca
023600990122     c*
023700990122     C     Keynca        chain     FNDCT000                           31
023800990122     c* faccio direttamente la chiusura della CA
023900990122     c                   exsr      SUB_coch
024000990122     c*
024100990122     c                   goto      fine
024200990122     c*
024300990122     c                   endif
024400990122     c*
024500990122     c* GESTIONE CHIUSURA RIAPERTURA A VIDEO
024600990122     c*
024700981013     C                   do        *hival
024800980623
024900980521      * Emissione VIDEO1
025000981013     c                   write     FI22t01
025100981013     c                   write     FI22z01
025200160913     c                   if        *in28
025300160913     c                   eval      *in28 = *off
025400160913     c                   write     FI22d01
025500160913     c                   eval      *in28 = *on
025600160913     c                   endif
025700981013     c                   exfmt     FI22d01
025800980923
025900980923     c                   setoff                                       284041
026000990419     c                   setoff                                       422190
026100980923     c                   clear                   v1cmsg
026200980623
026300980525      * f3=Fine
026400981021     c   kc              eval      o22f03 = 'S'
026500981013     c   kc              leave
026600980623
026700981014
026800980521      * f18=Cambio P.O. in gestione
026900980522     c   ks              eval      *in05 = *on
027000981013     c   ks              iter
027100980623
027200980521      * 05 on --> controllo P.O. in gestione
027300980521     c     *in05         ifeq      *on
027400980521     C                   exsr      ctrfgs
027500981013     c   28              iter
027600980608     c                   eval      *in05 = *off
027700980521     c                   end
027800980623
027900050512      * Controlli  videata - controllo C.A.
028000980521     c                   exsr      ctrd01
028100981105      * se è acceso il 21 o il 28 ritotrno ad emettere il formato video
028200050331     c                   if        *in28 or *in21 or *in90
028300981105     c                   iter
028400981105     c                   endif
028500981015      * f10=Note C.A.
028600981015     C     *INKJ         IFEQ      *ON
028700981015     c                   exsr      SUB_F10
028800981015     C                   iter
028900981015     c                   ENDIF
029000981015
029100981015      * f14=Interrogazione C.A.
029200981015     C     *INKN         IFEQ      *ON
029300981015     c                   exsr      SUB_F14
029400981015     C                   iter
029500981015     c                   ENDIF
029600050512      * Controlli  videata - altri controlli
029700050512     c                   exsr      CtrD01_b
029800050512      * se è acceso il 21 o il 28 ritotrno ad emettere il formato video
029900050512     c                   if        *in28 or *in21 or *in90
030000050512     c                   iter
030100050512     c                   endif
030200990511      * Se 20 acceso si tratta di una riapertura per rifiuto assegno di traenza
030300050331      * deve essere spento il 22
030400050331    1c                   IF        *in20 = *on  and *in22 = *off
030500990511      *   Se la C.A. resta in gestione alla sede imposto una fase fittizia x il ritorno al pagamento
030600990511    3c                   If        v1crit = 'S'
030700990511     c                   eval      wfaca = writi
030800990511     c                   Else
030900990511      *   Se deve tornare in gestione alla filiale richiamo il pgm rifiuto liquidazione transattiva
031000981105     c                   exsr      SUB_RIFIU
031100990511      *      x F12 riemetto videata, se confermata o premuto F3 esco
031200990511    4c                   If        o00f12 = 'S'
031300990312     c                   iter
031400990511    4c                   Else
031500990312     c                   leave
031600990511    4c                   Endif
031700990511      *
031800990511    3c                   Endif
031900990511    1c                   ENDIF
032000050331      *
032100050331      * se si tratta di un rifiuto assegno di traenza da causale 17 (22 acceso ) imposto
032200050331      * la fase 690 ristampa progetto di liquidazione)
032300050331     c                   If        *in22 = *on
032400050331     c                   eval      wfaca = wripl
032500050331     c                   endif
032600080114      *
032700080115      * Se chiusura con causale "Bonifico Manuale" richiede anche
032800080114      *   l'importo liquidato
032900080114     c                   clear                   $Video
033000080114if  1c                   IF        NOT  *in08
033100080115     c                             and  V1Ccch = WchBM
033200080114     c                   eval      $Video = 'D2'
033300080114do  2c                   dow       $Video = 'D2'
033400080114     c                   exsr      GesD02
033500080114e   2c                   enddo
033600080114if  2c                   if        O22f03 = 'S'
033700080114     c                   leave
033800080114e   2c                   endif
033900080114e   1c                   ENDIF
034000990511      *
034100981014      * Se non ho confermato o ci sono errori riemetto video
034200981015     c                   IF        *INKF = *ON
034300980910     c                   exsr      SUB_confer
034400981015     c                   leave
034500980908     c                   ELSE
034600981013     c                   iter
034700980908     c                   ENDIF
034800981014      *
034900981014     c                   enddo
035000980623
035100981021     c                   clear                   dsdn10
035200981021     c                   movel     'C'           i10tla
035300981021     c                   movel     dsdn10        kpjbu
035400981021     c                   call      'FIDN10R'
035500981021     c                   parm                    kpjba
035600981021      *
035700010508     c                   if        %OPEN(fidn22d)
035800990122     c                   close     fidn22d
035900010508     c                   endif
036000990122      *
036100990122     c     fine          tag
036200010508      *
036300981021     c                   movel     fidn22ds      kpjbu
036400981021
036500981029     c                   clear                   dsbs02
036600981029     c                   movel     'C'           t02tla
036700981029     c                   call      'TIBS02R'
036800981029     c                   parm                    kpjba
036900981029     c                   parm                    dsbs02
037000980907     c                   EVAL      *INLR = *ON
037100980521      **********************************************************************
037200980521      * CONTROLLO IL P.O. IN IN GESTIONE
037300980521      **********************************************************************
037400980521     C     CTRFGS        beGSR
037500980521      *
037600980521     c                   clear                   v1dfgs
037700980609     C                   clear                   FNLV50DS
037800980521     C                   MOVEL     KNMUS         D50PRU
037900980521     C                   MOVEL     V1cfgs        D50FGS
038000170427     c                   eval      d50tcn='E'
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
048900170427    3c                   if        *in10 = *off
049000170427     C     DCTgfc        LOOKUP    L6                                     34
049100170427    4c                   if        *in34 and simfel>0
049200170427     c                   setoff                                       32
049300170427     c     DCTlna        lookup    L6                                     32
049400170427    5c                   if        *in32 and d50val_a='N'
049500170427     c                   setoff                                           32
049600170427    5c                   endif
049700170427     c
049800180105     c                   if        not *in32
049900180105     c     w§DCTlnpc     lookup    L6                                     32
050000170427    5c                   if        *in32 and d50val_p='N'
050100170427     c                   setoff                                       32
050200170427    5c                   endif
050300180105    5c                   endif
050400180105
050500170427    5c                   if        not *in32
050600170427     c                   setoff                                       34
050700170427    5c                   endif
050800170427    4c                   endif
050900170427     c
051000170427    3c***                if        v1cfgs <> dctgfc
051100981014     C* verifico se si trova nella sua £6
051200170427     c***  dctgfc        lookup    L6                                     30
051300170427     c**n30              seton                                        2841
051400170427     c  n34              seton                                        2841
051500990712     C   28              movel     MSG(4)        V1Cmsg
051600990712     c   28              goto      Ectrd01
051700170427    3c*****              ENDIF
051800981030     c                   endif
051900050512     c*
052000050512    2c                   ENDIF
052100050512    1C                   ENDIF
052200160913      *
052300160913      /free
052400160913
052500160913         // -?Eventuale visualizzazione estremi della Pratica?
052600160913         if  V1Ccch <> WchBB  or
052700160921             DCTfca <  wPrLi  or
052800160921             *in28  =  *on;
052900160913           leavesr;
053000160913         endif;
053100160913
053200160913         *in23 = *on;
053300160914
053400160914         // -?Pratica affidata ad A.I.G.?
053500160914         *in71 = (§DCTaffi = 'A');
053600160914         // -?Pratica Assicurativa?
053700160914         *in72 = (DCTfpr  <> 'T'  and  §DCTpFra <> 'F');
053800160914         // -?Liquidazione Transattiva?
053900160914         *in73 = (DCTfpr   = 'T'  and  §DCTpFra <> 'F');
054000160914         // -?Pratica in Franchigia?
054100160915         *in74 = (§DCTpFra = 'F');
054200160921
054300160921         // -?Eventuale visualizzazione ALERT x anno trascorso dalla fase 700?
054400160921         If  V1Ccch = WchBB  and  DCTfca = wPrLi;
054500160921
054600160921           setGT     ( Kaac : Kfil : Knca )  FNDCF000;
054700160921           readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
054800160921           DoW  NOT %eof( FNDCF01L)  and  DCFfca <> wPrLi;
054900160921             readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
055000160921           EndDo;
055100160921
055200160921           if  NOT %eof( FNDCF01L )  and  DCFfca = DCTfca;
055300160921             *in24 = ( DCFdfc <= %dec( %date() - %years(1) ) );
055400160921           endif;
055500160921
055600160921         EndIf;
055700160921
055800160921         // -?Eventuale visualizzazione dati della Chiusura?
055900160921         If  DCTfca = wChCA;
056000160921
056100160921           *in70 = *on;
056200160921
056300160921           if  DCTdch > *zero;
056400160921             WdataEUR = %date( DCTdch : *iso );
056500160921             V1Odch   = %dec( WdataEUR );
056600160921           endif;
056700160921
056800160921           if  DCTcch > *blank;
056900160921             V1Occh = DCTcch;
057000160921             clear  dsBS02;
057100160921             T02mod = 'C';
057200160921             T02cod = 'CCH';
057300160921             T02ke1 = DCTcch;
057400160921      /end-free
057500160921     c                   call      'TIBS02R'
057600160921     c                   parm                    KPJBA
057700160921     c                   parm                    dsBS02
057800160921      /free
057900160921             if  T02err = *blank;
058000160921               dCCH    = T02uni;
058100160921               V1OcchD = §CCHdesc;
058200160921             endif;
058300160921           endif;
058400160921           // -?Verifica se è già stata fatta almeno un'estrazione?
058500160921           *in75 = ( §DCTpaga = 'C' );
058600160921           // -?Verifica se è già stato fatto il pagamento?
058700160921           *in76 = ( §DCTpaga = 'P' );
058800160921
058900160921         EndIf;
059000160915
059100160915         if  Not *in71  and  Not *in72  and
059200160915             Not *in73  and  Not *in74;
059300160915           *in23 = *off;
059400160921           *in24 = *off;
059500160915           leavesr;
059600160915         endif;
059700160913
059800160914         // -?Beneficiario pagamento C.A.?
059900160914         //  ?(SE Liquidazione Transattiva o Pratica in Franchigia)?
060000160914         if  *in73  or  *in74;
060100160914           chain(N)  ( Kaac : Kfil : Knca : 'P' )  FNDKA000;
060200160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
060300160914             V1Cpli = DKAksc;
060400160914             V1Dpli = DKArag;
060500160914           endif;
060600160914         // -?Intestatario Progetto Liquidazione?
060700160914         else;
060800160914           chain(N)  ( Kaac : Kfil : Knca : 'I' )  FNDKA000;
060900160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
061000160914             V1Cpli = DKAksc;
061100160914             V1Dpli = DKArag;
061200160914           endif;
061300160914         endif;
061400160913
061500160913
061600160921         // -?Altri dati del Progetto di Liquidazione:?
061700160913         if  Not %open(FNDCL01L);
061800160913           open  FNDCL01L;
061900160913         endif;
062000160913         chain(N)  ( Kaac : Kfil : Knca )  FNDCL000;
062100160915
062200160915         If  %found(FNDCL01L);
062300160913
062400160915           // -?Data Stampa Progetto di Liquidazione?
062500160915           if  DCLdds > *zeros;
062600160915             WdataEUR = %date( DCLdds : *iso );
062700160915             V1Cdds   = %dec( WdataEUR );
062800160915           endif;
062900160915
063000160915           // -?Data Accettazione Progetto di Liquidazione?
063100160915           if  DCLdpl > *zeros;
063200160915             WdataEUR = %date( DCLdpl : *iso );
063300160915             V1Cdpl   = %dec( WdataEUR );
063400160915           endif;
063500160915
063600160915           // -?Importo Liquidato?
063700160915           if  DCTfpr = 'T';
063800160915             //*·V2Cipl = DCLipl;
063900160915             //*·V2Cvpl = DCLvpl;
064000160915           else;
064100160915             V1Cipp = DCLipl;
064200160915             V1Cvpp = DCLvpl;
064300160915             //*·// -?Se pratica trattenuta da cliente e?
064400160915             //*·//     ?pagamento già avvenuto?
064500160915             //*·//  ?=> imposto come importo liquidato la differenza?
064600160915             //*·if  DCLipl <= DCLipt;
064700160915             //*·  clear  V2Cipl;
064800160915             //*·  clear  V2Cvpl;
064900160915             //*·else;
065000160915             //*·  V2Cipl = DCLipl - DCLipt;
065100160915             //*·  V2Cvpl = DCLvpl;
065200160915             //*·endif;
065300160915           endif;
065400160915
065500160915           // -?Data valuta pagamento?
065600160915           if  DCLvlp > *zeros;
065700160915             WdataEUR = %date( DCLvlp : *iso );
065800160915             V1Cvlp   = %dec( WdataEUR );
065900160915           endif;
066000160915
066100160915         EndIf;
066200160913
066300160913      /end-free
066400050512
066500050512     c     Ectrd01       ENDSR
066600050512      **********************************************************************
066700050512      * Altri CONTROLLI VIDEO 1
066800050512      **********************************************************************
066900050512     C     CtrD01_b      BEGSR
067000020128      *
067100020128      * Verifico se C.A. con flag di franchigia = 'F'
067200020128      *                  e  causale di chiusura = '15'
067300020128     c                   if            §DCTpfra = 'F'
067400020226     c                             and DCTcch   = WchFR
067500020128     c                   eval      $Franch = *on
067600020128     c                   else
067700020128     c                   eval      $Franch = *off
067800020128     c                   endif
067900981016     c*
068000981016    3c                   if        *in08 = *off
068100981016     c* controllo nel caso di chiusura CA
068200981016     c                   exsr      SUB_ctrch
068300981016   x3c                   else
068400981016     c* controllo nel caso di riapertura CA
068500981016     c                   exsr      SUB_ctrri
068600981016    3c                   endif
068700050512     c   28              goto      Ectrd01b
068800980923
068900050512     c     Ectrd01b      ENDSR
069000981016      ****************************************************************
069100981016      * SUB_CTRCH Controlli di chiusura CA
069200981016      ****************************************************************
069300981016     C     SUB_CTRCH     BEGSR
069400981016      *
069500981016     c* controllo se C.A. già chiusa
069600981016    1c                   if        dctdch > 0
069700981016     c                   seton                                        2841
069800981016     C                   movel     MSG(5)        V1Cmsg
069900981016    1c                   ENDIF
070000981016      *
070100981016      * Controllo la causale di chiususra
070200981016    1c                   if        *in28 = *off
070300981030    2c                   if        v1ccch = '  '
070400981016     c                   seton                                        4228
070500981016     c                   movel     msg(3)        v1cmsg
070600981016     c                   goto      ech01
070700981016    2c                   endif
070800050126      * solo la sede
070900050511    2c                   if           (V1Ccch = WchBA
071000050511     c                             or  V1Ccch = WchBB)
071100050511     c                             and *in07  = *off
071200050126     c                   seton                                        4228
071300050126     c                   movel     msg(16)       v1cmsg
071400050126     c                   goto      ech01
071500050126    2c                   endif
071600050512      *
071700050512      * SOLO IN SEDE controllo la causale di spedizione 17/18
071800050512  1.5c                   if        *in07
071900050511      * reperisco la data spedizione
072000050511     c                   exsr      ChaTITAS
072100050511      * causale chiusura 17 errata se data spedizione >  17/04/2005
072200050511    2c                   if            (DCTaas * 10000 + TASmgs)
072300050511     c                              >  C_DataChiusLim
072400050511     c                             and V1Ccch = WchBA
072500050511     c                   seton                                        2842
072600050512     c                   eval      V1Cmsg = %replace('precedente il' :
072700050511     c                                      MSG(18) :
072800050512     c                                      %scan('&PRE/SUC':MSG(18)))
072900050511     c                   goto      ech01
073000050511    2c                   endif
073100050511      * causale chiusura 18 errata se data spedizione <= 17/04/2005
073200050511    2c                   if            (DCTaas * 10000 + TASmgs)
073300050511     c                              <= C_DataChiusLim
073400050511     c                             and V1Ccch = WchBB
073500050511     c                   seton                                        2842
073600050512     c                   eval      V1Cmsg = %replace('successiva al' :
073700050511     c                                      MSG(18) :
073800050512     c                                      %scan('&PRE/SUC':MSG(18)))
073900050511     c                   goto      ech01
074000050511    2c                   endif
074100050512  1.5c                   endif
074200050512      *
074300981016      * controllo la causale di chiusura
074400981016    2c                   if        v1ccch <> '  '
074500981016      *
074600981016     C     '?'           SCAN      v1Ccch                                 31
074700981016      *
074800981016    3C                   IF        *IN31 = *ON
074900990419     c                   seton                                        90
075000981016     c                   clear                   dsbs02
075100981016     c                   movel     'R'           t02mod
075200981016     c                   movel     knsif         t02sif
075300981016     C                   movel     'CCH'         t02cod
075400981016      *
075500981016     C                   CALL      'TIBS02R'
075600981016     c                   parm                    KPJBA
075700981016     c                   parm                    dsbs02
075800981016      *
075900981016    4c                   if        t02err =  *blanks
076000981016     C                   MOVEL     t02ke1        v1ccch
076100981016    5C                   IF        v1ccch <> *BLANKS
076200981016     c                   movel     t02uni        v1dcch
076300990311     c                   clear                   dcch
076400990311     C                   MOVEL     T02UNI        dcch
076500990311     c* verifico se causale si può usare a video
076600990311     c                   if        §cchgevi <> 'S'
076700990311     c                   movel     msg(10)       v1cmsg
076800990311     c                   seton                                        4228
076900990311     c                   goto      ech01
077000990311     c                   endif
077100990311     c*
077200981016    5C                   ENDIF
077300981030     c                   else
077400981030     c                   movel     t02msg        v1cmsg
077500981030     c                   goto      ech01
077600981016    4c                   endif
077700981016      *
077800981016     C                   GOTO      Ech01
077900981016   x3C                   Else
078000981016      *   Controlli di validità'
078100981016      *
078200981016     c                   clear                   dsbs02
078300981016     C                   MOVEL     'C'           t02mod
078400981016     C                   MOVEL     knsif         t02sif
078500981016     C                   MOVEL     'CCH'         t02cod
078600981016     C                   MOVEL(P)  v1ccch        t02ke1
078700981016      *
078800981016     C                   CALL      'TIBS02R'
078900981016     C                   PARM                    KPJBA
079000981016     C                   PARM                    dsbs02
079100981016      *
079200981016    4C                   if        t02err = *BLANKS
079300981016     C                   MOVEL     T02UNI        v1dcch
079400990311     c                   clear                   dcch
079500990311     C                   MOVEL     T02UNI        dcch
079600990311     c* verifico se causale si può usare a video
079700990311     c                   if        §cchgevi <> 'S'
079800990311     c                   movel     msg(10)       v1cmsg
079900990311     c                   seton                                        4228
080000990311     c                   goto      ech01
080100990311     c                   endif
080200990311     c*
080300981016     c                   else
080400990311     c*
080500981016     c                   movel     msg(3)        v1cmsg
080600981016     c                   seton                                        4228
080700981016     c                   goto      ech01
080800981016    4C                   endif
080900981016    3C                   endif
081000981016     c                   else
081100981016     c                   clear                   v1dcch
081200981016    2C                   endif
081300981016    1C                   endif
081400050126     c* controllo se C.A. in fase minore del progetto di liquidazione
081500050511    1c  n28              if             DCTfca < Wprli
081600050511     c                             and (V1Ccch = WchBA
081700050511     c                              or  V1Ccch = WchBB)
081800050126     c                   seton                                        2842
081900050126     C                   movel     MSG(15)       V1Cmsg
082000050929     c                   leavesr
082100050126    1c                   ENDIF
082200050929      *
082300050929      * controlli sull'utilizzo della causale in base ai flag in DCCH:
082400050929      * - per le causali di pagamento: ci deve essere compatibilità
082500131011      *   con l'affidamento (o meno) ad A.I.G. già memorizzato su
082600050929      *   FNDCT00F (in DCTFLO - ds DDCT01)
082700111223if  1c****               if            §CCHpaga  = 'S'
082800111223     c****                         and §DCTaffi  = 'A'
082900111223     c****                         and V1Ccch   <> WchBB
083000111223     c****               eval      V1Cmsg   = %trim(Msg(20))
083100111223     c****               seton                                        2842
083200111223     c****               leavesr
083300111223e   1c****               endif
083400050929      * - per le causali utilizzabili solo in sede: verifico la
083500050929      *   compatibilità con il profilo utente
083600051025sel 1c                   select
083700050929      * - - utilizzabile da tutti gli utenti
083800051025w   1c                   when      §CCHutil = *blanks
083900050929      * - - utilizzabile dai soli utenti di sede
084000051025w   1c                   when          §CCHutil =  'S'
084100050929     c                             and DUTlpo   <> 'S'
084200050929     c                   eval      V1Cmsg = %trim(Msg(21))
084300050929     c                                    + ' sede'
084400050929     c                   seton                                        2842
084500050929     c                   leavesr
084600050929      * - - utilizzabile dai soli utenti di p.o.
084700051025w   1c                   when          §CCHutil =  'P'
084800050929     c                             and DUTlpo   =  'S'
084900050929     c                   eval      V1Cmsg   = %trim(Msg(21))
085000080114     c                                      + ' filiale'
085100050929     c                   seton                                        2842
085200050929     c                   leavesr
085300051025e   1c                   endsl
085400981130     c*
085500981016
085600981016     c     ech01         ENDSR
085700981016      ****************************************************************
085800981016      * SUB_CTRRI Controlli di riapertura CA
085900981016      ****************************************************************
086000981016     C     SUB_CTRRI     BEGSR
086100981016      *
086200981016     c* controllo se C.A. non chiusa  in caso di riapertura C.A.
086300981130    1c                   if        dctdch = 0
086400981016     c                   seton                                        2841
086500981016     C                   movel     MSG(6)        V1Cmsg
086600981016     c                   goto      eri01
086700981016    1c                   ENDIF
086800050922      *
086900131011     c* controllo se C.A. già in fase di richiesta recupero ad A.I.G.
087000050922     C     Keynca        chain     FNDRA01L
087100050922    1c                   if        %found(fndra01l) and dradrr > 0
087200050922     c                   seton                                        2841
087300050922     C                   movel     MSG(19)       V1Cmsg
087400050922     c                   goto      eri01
087500050922    1c                   ENDIF
087600990712     c*
087700990712     c* calcolo la data massima per riapertura CA
087800990712     c     *iso          move      dctdch        wdata
087900050218     c**!!!              adddur    4:*m          wdata
088000050218     c                   adddur    §5abca:*d     wdata
088100990712     c     *iso          move      wdata         wrkdta
088200010926     c*
088300010926     c* se non riapribile in modo normale:
088400050218     c* solo in sede: riapertura consentita oltre i giorni da tabella 5A
088500010926     c*           ma  non se bolla antecedente il 1° settembre 1999.
088600010926     c* (modifico la data massima per riapertura CA - wrkdta)
088700010926     c                   eval      $over4m = *off
088800011008    1c                   if        *in07         and
088900010926     c                             dateu   > wrkdta
089000010926     c                   eval      $over4m = *on
089100011008     c                   exsr      ChaTITAS
089200011008     c* ok oltre il 01/09/1999
089300011008    2c                   if         DCTaas  > 1999   or
089400011008     c                             (DCTaas  = 1999  and  TASmgs >= 0901)
089500011008     c                   eval      wrkdta  = *hival
089600011008     c                   else
089700011008     c                   seton                                        2841
089800011008     c                   movel     MSG(8)        V1Cmsg
089900011008     c                   goto      eri01
090000011008    2c                   endif
090100011008    1c                   endif
090200010926     c*
090300050218     c* controllo se CA chiusa da più giorni da tab. 5A
090400990712     c                   if        dateu > wrkdta
090500990712     c                   seton                                        2841
090600050218     c                   move      §5abca        w003a             3
090700050218     C                   movel     MSG(14)       V1Cmsg
090800050218     c                   Eval      %subst(v1cmsg:48:3) = w003a
090900990712     c                   goto      eri01
091000990712    1c                   endif
091100990712     c
091200990311     c* controllo se la C.A. chiusa  non sia in fase di contabilizzazione
091300990311    1c                   if        §dctpaga = 'C'
091400990311     c                   seton                                        2841
091500990311     C                   movel     MSG(11)       V1Cmsg
091600990311     c                   goto      eri01
091700990311    1c                   endif
091800050331      * verifico se la CA chiusa può essere riaperta solo da un utente autorizzato
091900050331      *
092000050331     c                   clear                   dsbs02
092100050331     C                   MOVEL     'C'           t02mod
092200050331     C                   MOVEL     knsif         t02sif
092300050331     C                   MOVEL     'CCH'         t02cod
092400050331     C                   MOVEL(P)  dctcch        t02ke1
092500050331      *
092600050331     C                   CALL      'TIBS02R'
092700050331     C                   PARM                    KPJBA
092800050331     C                   PARM                    dsbs02
092900050331      *
093000050331     c                   clear                   dcch
093100050331     C                   MOVEL     T02UNI        dcch
093200050331      * se richiesto il controllo dell'utente escludo gli utenti EDP
093300050331     c                   If        §cchprot = 'S' and %subst(knmus:1:3)<>'EDP'
093400050331      *
093500160914     c     knmus         lookup    ute                                    33
093600050331     c                   if        not %found
093700050331     c                   seton                                        2841
093800050331     C                   movel     MSG(17)       V1Cmsg
093900050331     c                   goto      eri01
094000050331    1c                   endif
094100050331    1c                   endif
094200990311     c* controllo se C.A. in fase di pagamento non deve essere per pratica assicurativa
094300050420    1c                   if        §dctpaga = 'P'  and
094400050331     c                             (dctcch = wchpa )
094500050420     c*                  seton                                        2841
094600050420     C*                  movel     MSG(12)       V1Cmsg
094700050420     c*                  goto      eri01
094800990311    1c                   endif
094900990311     c* Controllo se il numero dei colli in testata CA è uguale al numero dei colli nel dettaglio
095000990311     c                   exsr      SUB_COLLI
095100990311     c*
095200990311     c* se numero colli differenti non si può aprire
095300990311    1c                   if        dctncn <> wnco
095400990311     c                   seton                                        2841
095500990311     C                   movel     MSG(13)       V1Cmsg
095600990311     c                   goto      eri01
095700990311    1c                   endif
095800981016     c*
095900981016     c* controllo chi sono io
096000981230     c                   if        dctgfc = W§dctlnpc
096100981016     c                   movel     'P'           chisono                        * PARTENZA
096200040722     c                   eval      giaaas = dctaas
096300040722     c                   eval      gialnp = dctlnp
096400040722     c                   eval      gianrs = dctnrs
096500040722     c                   eval      giansp = dctnsp
096600981016     c                   endif
096700981016     c                   if        dctgfc = dctlna
096800981016     c                   movel     'A'           chisono                        * ARRIVO
096900040722      * verifico se ho spedizione figlia su FNDCK prendo quella
097000040722     c     keynca        chain     fndck01l
097100040722     c                   if        %found(fndck01l)
097200040722     c                   eval      giaaas = dckaas
097300040722     c                   eval      gialnp = dcklnp
097400040722     c                   eval      gianrs = dcknrs
097500040722     c                   eval      giansp = dcknsp
097600040722     c                   else
097700040722     c                   eval      giaaas = dctaas
097800040722     c                   eval      gialnp = dctlnp
097900040722     c                   eval      gianrs = dctnrs
098000040722     c                   eval      giansp = dctnsp
098100040723     c                   endif
098200040723      *
098300981016     c                   endif
098400981016     c                   if        dctgfc = 046
098500981016     c                   movel     'S'           chisono                        * SEDE
098600981016     c                   endif
098700981016     c*
098800040722     c* sono l' ARRIVO o la PARTENZA controllo la giacenza in ARRIVO
098900040722     c                   if        chisono <> 'S'
099000010508      *
099100050404     C                   if        not %OPEN(Tigcp21l)
099200050404     C                   OPEN      Tigcp21l
099300010508     c                   endif
099400010508      *
099500050404     C     kgiac         chain     Tigcp21l                           32
099600050404     c                   if        not *in32 and gcpfas < 40
099700981016     c                   seton                                        2841
099800981021     C                   movel     MSG(7)        V1Cmsg
099900981016     c                   goto      eri01
100000981016     c                   endif
100100010508      *
100200050404     C                   if        %OPEN(Tigcp21L)
100300050404     c                   close     Tigcp21l
100400010508     c                   endif
100500010508      *
100600010508      * controllo la data consegna e il numero Distinta consegna
100700010508     C                   if        not %OPEN(FNARB01L)
100800010508     C                   OPEN      FNARB01L
100900010508     c                   endif
101000010508      *
101100990311     C     karb          chain     fnarb01l                           32
101200981230     c* controllo se c'è un flag blocco di spedizione diverso da "D", da " " e da "*"
101300981230     c                   if        arbfbc <> *blanks  and  arbfbc <> '*'
101400981021     c                   movel     msg(9)        V1Cmsg
101500981019     c                   seton                                        2841
101600981019     c                   goto      eri01
101700981019     c                   endif
101800010508      *
101900010508     C                   if        %OPEN(FNARB01L)
102000010508     C                   close     FNarb01L
102100010508     c                   endif
102200010508      *
102300981016     c                   endif
102400981016     c*
102500981019      * SE NON CI SONO ERRORI
102600981019     c* cerco la fase precedente la chiusura CA
102700981202     c     keynca        setgt     fndcf01l
102800981019     c     keynca        readpe    fndcf01l                               31
102900981202     c                   dow       dcffca = wchca
103000981019     c     keynca        readpe    fndcf01l                               31
103100981202     c                   enddo
103200981130     c  n31              eval      wfaca = dcffca
103300981105     c*
103400981105     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
103500990511     c* liquidazione transattiva : se è così imposto se si tratta di un rifiuto assegno di traenza
103600990311     c                   if        chisono  = 'S'  and  §dctpaga = 'P' and
103700990511     c                             dcffca = wcolt  and  *in20 = *off
103800981105     c                   seton                                        2021
103900981105     c                   endif
104000990430     c*
104100990430     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
104200090925     c* liquidazione transattiva : se è così non imposto + la fase di esito ma quella di av.danno
104300090925     c* liquidazione transattiva
104400990430     c                   if        chisono  = 'S'  and  §dctpaga = ' ' and
104500990511     c                             dcffca = wcolt  and  *in20 = *off
104600090925     c*******            eval      wfaca  = wesit
104700090925     c                   eval      wfaca  = wavdt
104800990430     c                   endif
104900050331      * se sto riaprendo una CA pagata in surroga visualizzo i dati dell'assegno
105000050511     c                   if            (DCTcch   = WchBA
105100051025     c                              or  DCTcch   = WchBB)
105200050511     c                             and  §DCTpaga = 'P'
105300050511     c                             and  *in20    = *off
105400050331     c                   seton                                        202122
105500050331     c                   endif
105600050401      * per 21 acceso visualizzo gli estremi del pagamento
105700050401      *
105800050401     c                   If        *in21
105900050401
106000050401     C                   if        not %OPEN(Fndcl01L)
106100050401     c                   open      fndcl01l
106200050401     c                   endif
106300050401      *
106400050401     c     keynca        chain(n)  fndcl01l
106500050401     c                   If        %found(fndcl01l)
106600050401      * Data emissione Assegno
106700050401     c                   if        DCLdea > *zeros
106800050401     c     *iso          move      DCLdea        Wdataeur
106900050401     c     *eur          movel     Wdataeur      V1Cdea
107000050401     c                   endif
107100050401      *
107200050401      * Numero  Assegno
107300161206      * o Bonifico
107400050401     c                   movel     dclasn        v1casn
107500161206     c                   clear                   V1Dasn
107600161206     c                   SELECT
107700161206     c                   WHEN      dclasn = *blanks
107800161206     c                   eval      V1Dasn = 'Ass/Bon'
107900161206     c                   WHEN      %subst(dclasn:1:1) = 'B'
108000161206     c                   eval      V1Dasn = 'Bonifico'
108100161206     c                   OTHER
108200161206     c                   eval      V1Dasn = 'Assegno'
108300161206     c                   ENDSL
108400050401      * importo liquidato
108500050401     c                   z-add     DClipl        V1Cipl
108600050401     c                   movel     dclvpl        v1cvpl
108700050401
108800050401     c                   endif
108900050401
109000050401     c                   if        %OPEN(Fndcl01L)
109100050401     c                   close     fndcl01l
109200050401     c                   endif
109300050401
109400050401     c                   endif
109500120507
109600120507      *?Controllo se si sta riaprendo un Porto Franco Import EEX chiuso con CAU 06
109700120507     c                   eval      wPFIEEX = *off
109800120507     c                   IF        §DCTport = 'F' and §DCTtisp = 'I' and
109900120507     c                              DCTcch = wCHEE
110000120507     c                   eval      wPFIEEX = *on
110100120507      *?Recupero la data della bolla
110200120507     c                   exsr      ChaTITAS
110300120507     c                   eval      Wdaspe = (TASaas * 10000) + TASmgs
110400120507     c                   ENDIF
110500981016
110600981016     c     eri01         ENDSR
110700981014      ****************************************************************
110800981014      * F10 = GESTIONE NOTE
110900981014      ****************************************************************
111000981014     C     SUB_F10       BEGSR
111100981014      *
111200981015     c*
111300981015     c                   eval      aac = kaac
111400981015     c                   eval      fil = kfil
111500981015     c                   eval      nca = knca
111600981015     c*
111700981014     C                   clear                   DSDN10
111800981015      * calcolo data apertura C.A.
111900981015     C                   eval      wdataca = (DCTaac * 10000) + DCTmgc
112000981015     c*
112100981015     C                   CLEAR                   WLBDAT
112200981015     C                   Z-ADD     wdataca       G02inv
112300981015     C                   movel     '3'           G02err
112400981015     C                   CALL      'XSRDA8'
112500981015     C                   PARM                    WLBDAT
112600981015     c                   z-add     g02dat        wdataca
112700981015     c*
112800981015     c                   eval      i10dta = wdataca
112900981014     c                   movel     'N'           i10trc
113000981014     c                   movel     'C'           i10tpd
113100981014      *
113200080115     c                   if        $Video = 'D2'  and  *inKF
113300080115     c                   eval      I10flm = 'V'
113400080115     c                   else
113500981015     c                   movel     'M'           i10flm
113600080115     c                   endif
113700981130     c                   movel     wchca         i10nks
113800981014     c                   movel     dsnumca       i10nkt
113900981014      *
114000981014     c                   movel(P)  dsdn10        kpjbu
114100981014     C                   CALL      'FIDN10R'
114200981014     C                   PARM                    kpjba
114300080115     c                   movel     kpjbu         dsdn10
114400981014      *
114500981014     c                   ENDSR
114600981015      ****************************************************************
114700981015      * F14 = INTERROGAZIONE C.A.
114800981015      ****************************************************************
114900981015     C     SUB_F14       BEGSR
115000981016      *
115100981016     c                   clear                   FIDN00DS
115200981016      *
115300981016     c                   movel     'I'           I00mod
115400981130     c                   movel     wchca         I00fca
115500981016     c                   z-add     dctgfc        I00fgs
115600981016     c                   z-add     dctaac        I00aac
115700981016     c                   z-add     dctfil        I00fil
115800981016     c                   z-add     dctnca        I00nca
115900981016     c                   z-add     dctaas        I00aas
116000981016     c                   z-add     dctlnp        I00lnp
116100981016     c                   z-add     dctnrs        I00nrs
116200981016     c                   z-add     dctnsp        I00nsp
116300981016     c                   z-add     dctlna        I00lna
116400981016     c                   movel     dcttad        I00tad
116500981230     c                   if        dctgfc = W§dctlnpc
116600981016     c                   movel     'P'           I00tpb
116700981016     c                   endif
116800981016     c                   if        dctgfc = dctlna
116900981016     c                   movel     'A'           I00tpb
117000981016     c                   endif
117100981016     c                   if        dctgfc = 046
117200981016     c                   movel     'S'           I00tpb
117300981016     c                   endif
117400981016      *
117500981016     c                   movel     FIDN00DS      kpjbu
117600981016      *
117700981016     c                   call      'FIDN01R'
117800981016     c                   parm                    kpjba
117900981016      *
118000981015     c                   endsr
118100990311      *****************************************************************
118200990311      * CONTROLLO DETTAGLIO COLLI
118300990311      *****************************************************************
118400990311     C     SUB_COLLI     BEGSR
118500990311
118600990311      * Lettura del dettaglio colli per verificare se uguale a quello indicato
118700990311      * in testata
118800990311     c                   clear                   wnco
118900990311     c*
119000990311     c     keynca        setll     fndcd01l
119100990311     c     keynca        reade(n)  fndcd01l                               32
119200990311      *
119300990311     c                   dow       not *in32
119400990311      *
119500990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
119600990412     c* e record non annullato
119700990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
119800990412     c                             and dcdatb = ' '
119900990311     c                   add       1             wnco
120000990311     c                   endif
120100990311q    c*
120200990311     c     keynca        reade(n)  fndcd01l                               32
120300990311     c*
120400990311     c                   enddo
120500990311     c*
120600990311     c                   endsr
120700980608      *****************************************************************
120800980923      * AGGIORNO ARCHIVI
120900980608      *****************************************************************
121000980923     C     SUB_confer    BEGSR
121100981007
121200981015      * note fase ca
121300080115      * (se NON già confermate in subr. "GesD02")
121400080115if  1c                   IF             *in08
121500080115     c                             or   V1Ccch <> WchBM
121600981015     C                   CLEAR                   DSDN10
121700981015     c                   z-add     Wdataca       i10dta
121800981015     C                   movel     'C'           I10FLM
121900080115     c                   eval      I10tpd = 'C'
122000981015     c                   movel(P)  dsnumca       i10nkt
122100981015     c                   movel     Wchca         i10nks
122200981015     c                   movel     'N'           i10trc
122300981015     c                   movel(P)  dsdn10        kpjbu
122400981015     C                   CALL      'FIDN10R'
122500981015     C                   PARM                    kpjba
122600080115e   1c                   ENDIF
122700050404      *
122800050404     c* chiusura C.A.
122900050404     c                   if        *in08 = *off
123000050404     c                   exsr      sub_coch
123100050404     c*
123200050404     c                   else
123300050404     c* riapertura
123400050404     c                   exsr      sub_cori
123500050404     c                   endif
123600981009      *
123700980923     c                   ENDSR
123800981014      *****************************************************************
123900981014      * AGGIORNO ARCHIVI   CHIUSURA C.A.
124000981014      *****************************************************************
124100981014     C     SUB_coch      BEGSR
124200020129      *
124300020129     c                   movel     DCTflo        DDCT01
124400020226      *
124500120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
124600120723     c                   clear                   §DCTspgd
124700120723     c                   movel     DDCT01        DCTflo
124800120723     c                   clear                   DCTpgd
124900981014      *
125000981014      * Imposto campi per trasmissione dati
125100981019     c                   z-add     dateu         DCTdch
125200981130     c                   z-add     wfaca         DCtfca
125300981014     c                   move      v1ccch        DCTcch
125400981222     c                   clear                   DCTft1
125500981222     c                   clear                   DCTft2
125600981014      *
125700981014      * Aggiorno testata C.A.
125800981014     c                   EXCEPT    UPDdct
125900981014      *
126000981014      * Aggiorno l dettaglio C.A.
126100981014     c                   EXSR      UPDDCD
126200050126      *
126300050511      * se sto chiudendo una pratica con causale 17 o 18 aggiorno FNDCL
126400080115      *                                            (o 19)
126500080114     c                   if            V1Ccch = WchBA
126600050511     c                             or  V1Ccch = WchBB
126700080115     c                             or  V1Ccch = WchBM
126800050126     c                   exsr      UPDDCL1
126900050126     c                   endif
127000981014      *
127100981014      * Scrivo fasi C.A.
127200981014     c                   EXSR      WRTDCF
127300051124      * Se programma non richiamato  e
127400131011      * Se chiusa C.A. legata ad Evento con data segnalazione ad A.I.G.
127500131011      *                               o in fase di rimborso A.I.G.
127600131011      *         o C.A. già affidata ad A.I.G. (con importo > §STDLAE)
127700131011      * => emetto window con msg: "C.A. per cui chiedere recupero A.I.G."
127800051018      *                           "Fotocopiare progetto liquidazione "
127900051124     c                   if        i22tri = '  '
128000080114      *
128100051124     c                   if        DCTnev   > *zeros
128200051020     c     K02DET01      chain(n)  FNDET000
128300051020     c                   if        NOT %found(FNDET01L)
128400051020     c                             or  DETatb <> *blanks
128500051020     c                   clear                   DETdtr
128600051020     c                   clear                   DETftr
128700051020     c                   endif
128800051117     c                   endif
128900051020     c                   if             DETdtr   > *zeros
129000051018     c                              or  DETftr   = 'S'
129100051020     c                              or  §DCTaffi = 'A'
129200051018     c                   exfmt     FI22W01
129300051018     c                   endif
129400981014      *
129500051124     c                   endif
129600070404
129700070404      * se filiale gestione non è sede
129800070404    1c                   if        v1cfgs <> 046
129900070404      * se filiale gestione è abilitata alle telefonate GEO
130000070404    2c                   if        wfilgeo = *on
130100070404      * se LNA della spedizione in £6
130200070404     c     dctlna        lookup    l6                                     31
130300170427    3c                   if        *in31 and d50val_a='S'
130400070404      * controllo la data consegna merce e la distinta consegna
130500070404     c                   if        not %open(fnarb01l)
130600070404     c                   open      fnarb01l
130700070404     c                   endif
130800070404     c     karb          chain(n)  fnarb01l
130900070404    4c                   if        %found(fnarb01l) and
131000070404     c                             arbndc = *zeros and arbdcm = *zeros
131100070404     c                   clear                   dslr02
131200070404     c                   eval      dr2aas = dctaas
131300070404     c                   eval      dr2lnp = dctlnp
131400070404     c                   eval      dr2nrs = dctnrs
131500070404     c                   eval      dr2nsp = dctnsp
131600070404     c                   eval      dr2fgs = v1cfgs
131700070404     c                   eval      dr2ric = '1'
131800070404     c                   eval      kpjbu = dslr02
131900070404     c                   call      'FNLR39R'
132000070404     c                   parm                    kpjba
132100070404     c                   eval      dslr02 = kpjbu
132200070404    4c                   endif
132300070404     c                   if        %open(fnarb01l)
132400070404     c                   close     fnarb01l
132500070404     c                   endif
132600070404    3c                   endif
132700070404    2c                   endif
132800070404    1c                   endif
132900051124      *
133000981014     c                   ENDSR
133100981014      *****************************************************************
133200981014      * AGGIORNO ARCHIVI   RIAPERTURA C.A.
133300981014      *****************************************************************
133400981014     C     SUB_cori      BEGSR
133500990311      *
133600990311      * Aggiorno il dettaglio C.A. che ha la data e causale chiusura uguale a quella della testata
133700990311     c     keynca        setll     fndcd01l
133800990311     c     keynca        reade     fndcd01l                               32
133900990311      *
134000990311     c                   dow       not *in32
134100990311      *
134200990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
134300990311     c*
134400990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
134500990311     c                   clear                   DCddch
134600990311     c                   clear                   DCdcch
134700990311     c                   clear                   DCdftr
134800990311     c                   update    fndcd000
134900990311     c                   endif
135000990311q    c*
135100990311     c     keynca        reade     fndcd01l                               32
135200990311     c*
135300990311     c                   enddo
135400120507      *?Se riapertura di un Porto Franco Import EEX chiuso con CAU 06
135500120507     c                   IF        wPFIEEX
135600120507      *?Cerco il codice del partner da impostare come beneficiario alternativo
135700120507     c                   clear                   FIDN40DS
135800120507     c                   eval      I40dsp = Wdaspe
135900120507     c                   eval      I40fil = dctlnp
136000120507     c                   call      'FIDN40R'
136100120507     c                   parm                    fidn40ds
136200120507      *?Ho trovato il codice del partner imposto la Fase e il beneficiario alternativo
136300120507     c                   IF        O40cod <> *zeros
136400120507     c                   eval      wfaca = wAVVD
136500120507     c                   eval      DCTksc = O40cod
136600120508      *?Devo anche recuperare il numero di pratica
136700120508     c                   IF        DCTprn = *zeros
136800120508     c                   exsr      NUMPRN
136900120508     c                   ENDIF
137000120509     c                   ENDIF
137100120507     c                   ENDIF
137200020131      *
137300981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
137400080123     c                   eval      wSAVcch = DCTcch
137500981014     c                   z-add     *zeros        DCTdch
137600981130     c                   z-add     wfaca         DCtfca
137700080701      * verifico se CA in fase 200 ormai obsoleta da Giugno 2008 imposto la filiale di gestione
137800080701      * in 046 sede per far eseguire la fase 230
137900080701     c                   if        dctfca = 200
138000080701     c                   eval      dctgfc = 46
138100080701     c                   endif
138200080701
138300981014     c                   clear                   DCTcch
138400981222     c                   clear                   DCTft1
138500981222     c                   clear                   DCTft2
138600120723      *
138700120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
138800120723     c                   clear                   DCTpgd
138900120723     c                   clear                   §DCTspgd
139000120723     c                   movel     DDCT01        DCTflo
139100020131      *
139200050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
139300020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
139400020131      *   causale "15"
139500020131      * => pulisco i flag per l'estrazione
139600050420      *   ADESSO PULISCO SEMPRE ***
139700990430      *
139800050420     c***                if        wfaca = writi or wfaca = wripl
139900050420    1c****                         or $Franch = *on
140000990430     c* pulisco il flag dctpaga nel campo dctflo
140100990430     c                   clear                   §dctpaga
140200990430     c                   movel     ddct01        dctflo
140300050420     c*****              endif
140400050218
140500050218      * Se sono decorsi i gg. da tab. 5A dalla data di chiusura
140600050218      * pulisco il campo dctdt2
140700050218    2c                   if        $over4m = *on
140800050218     c                   Clear                   dctdt2
140900050218    2c                   endif
141000020131      *
141100990430      * Aggiorno testata C.A.
141200990430     c                   EXCEPT    UPDdct
141300020131      *
141400050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
141500020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
141600020131      *   causale "15"
141700020131      * => pulisco i flag per l'estrazione
141800990430      *
141900050331     c                   if        wfaca = writi  or  wfaca = wripl
142000020131    1c                             or $Franch = *on
142100080123     c                             or wSAVcch = WchBM
142200990430      * Aggiorno testata C.A. importi solo per ritorno al pagamento
142300990430     c                   EXsr      UPDdcl
142400990430     c                   endif
142500050805      *
142600131011      * Ripulisco il flag per richiesta rimborso ad A.I.G. su FNDET00F
142700050805      *   (DETFTR) se quest'ultimo è già impostato a "P" o "N"
142800050805     c                   exsr      UpdDET
142900981014      *
143000981014      * Scrivo fasi C.A.
143100981014     c                   EXSR      WRTDCF
143200010926      *
143300050218      * Se sono decorsi i gg. della tab. 5A dalla data chiusura
143400090417      * aggiorno il file FnRIG00R - rigenerazione bolle x P.O.
143500010926    2c                   if        $over4m = *on
143600010926     c                   EXSR      WRTfirig
143700010926    2c                   endif
143800020128      *
143900050401      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
144000050401     c                   if        wfaca = writi  or  wfaca = wripl
144100050401      * scrivo note estremi aseegno rifiutato
144200050401     c                   EXsr      WRTDCS
144300050401     c                   endif
144400981014      *
144500981014     c                   ENDSR
144600990430      *****************************************************************
144700990430      * AGGIORNO FNDCL Testata C.A. Importi
144800990430      *****************************************************************
144900990430     C     UPDDCL        BEGSR
145000990430     c*
145100990430      * pulisco i campi relativi al pagamento ma lascio gli importi
145200020201      * => NON IN CASO DI FRANCHIGIA
145300990430      *
145400990430     c* apro e aggancio  il file liquidazione ca
145500010508      *
145600010508     C                   if        not %OPEN(Fndcl01L)
145700990430     c                   open      fndcl01l
145800010508     c                   endif
145900010508      *
146000990430     c     keynca        chain     fndcl01l                           30
146100010508      * pulisco  i dati relativi il pagamento (ABI/CAB, numero assegno, dta pagamento)
146200010508     c                   clear                   dclfic
146300990430     c                   clear                   dclabi
146400990430     c                   clear                   dclcab
146500990430     c                   clear                   dclasn
146600990430     c                   clear                   dcldea
146700020128      * pulisco le date relative alla stampa ed accettazione progetto di liquidazione
146800080114     c                   IF        $Franch = *on
146900020128     c                   clear                   DCLdds
147000020128     c                   clear                   DCLdpl
147100080114     c                   clear                   DCLipl
147200080114     c                   clear                   DCLvpl
147300080114     c                   ENDIF
147400080123      *
147500080123      * Ripulisco l'importo eventualmente liquidato
147600080123     c                   IF        wSAVcch = WchBM
147700080123     c                   clear                   DCLipl
147800080123     c                   clear                   DCLvpl
147900080123     c                   clear                   DCLvlp
148000080123     c                   ENDIF
148100080123      *
148200990430     c                   clear                   dclftr
148300080123     c  n30              update    fndcl000
148400010508      *
148500010508     C                   if        %OPEN(Fndcl01L)
148600010508     c                   close     fndcl01l
148700010508     c                   endif
148800150702
148900150702      /free
149000150702       //?Cancella rcd 'P' in FNDKA, beneficiario
149100150702         chain (kaac:kfil:knca:'P') FNDKA01L;
149200150702         IF  %found(FNDKA01L);
149300150702           delete FNDKA000;
149400150702         ENDIF;
149500150702      /end-free
149600990430     c*
149700990430     c                   endsr
149800050805
149900050805      **********************************************************************
150000050805      *  AGGIORNAMENTO TESTATA EVENTI
150100050805      **********************************************************************
150200050805     C     UpdDET        BEGSR
150300050805      *
150400131011      * Ripulisco il flag di RIMBORSO A.I.G. su FNDET00F
150500131011      * se  "P" = Ricezione totale del rimborso da A.I.G.
150600050805      *  o  "N" = Evento non fortuito che ha le CA pagate ma < 10000€
150700050805      *
150800050805     c     K02DET01      chain     FNDET000
150900050805      *
151000050805if  1c                   if        %found(FNDET01L)
151100050805     c                             and (DETftr = 'P'
151200050805     c                              or  DETftr = 'N')
151300050805     c                   clear                   DETftr
151400050805     c                   UPDATE    FNDET000
151500050805e   1c                   endif
151600050805      *
151700050805     C                   ENDSR
151800050126      *****************************************************************
151900050126      * AGGIORNO FNDCL data accettazione P.L.
152000050126      *****************************************************************
152100050126     C     UPDDCL1       BEGSR
152200050126      *
152300050126     c* apro e aggancio  il file liquidazione ca
152400050126      *
152500050126     C                   if        not %OPEN(Fndcl01L)
152600050126     c                   open      fndcl01l
152700050126     c                   endif
152800050126      *
152900050126     c     keynca        chain     fndcl01l                           30
153000080115      *
153100080115      * Imposto l'importo liquidato
153200080115     c                   IF        NOT  *in08
153300080115     c                             and  V1Ccch = WchBM
153400080123      *
153500080115     c                   if        *in30
153600080115     c                   clear                   FNDCL000
153700080115     c                   eval      DCLaac = Kaac
153800080115     c                   eval      DCLfil = Kfil
153900080115     c                   eval      DCLnca = Knca
154000080115     c                   endif
154100080123     c                   if        V2Cipl > *zeros
154200080115     c                   eval      DCLipl = V2Cipl
154300080115     c                   eval      DCLvpl = V2Cvpl
154400080115     c                   else
154500080115     c                   clear                   DCLipl
154600080115     c                   clear                   DCLvpl
154700080115     c                   endif
154800080123     c                   eval      DCLvlp = DateU
154900080123      *
155000080123     c                   ELSE
155100080123      *
155200050126     c                   z-add     dateu         dcldpl
155300080123      *
155400080123     c                   ENDIF
155500080123      *
155600050126     c                   clear                   dclftr
155700080115      *
155800080115     c                   select
155900080115     c                   when      NOT *in30
156000080115     c                   UPDATE    FNDCL000
156100080115     c                   when      *in30  and  DCLipl > *zeros
156200080115     c                   WRITE     FNDCL000
156300080115     c                   endsl
156400050126      *
156500050126     C                   if        %OPEN(Fndcl01L)
156600050126     c                   close     fndcl01l
156700050126     c                   endif
156800050126     c*
156900050126     c                   endsr
157000981014      *****************************************************************
157100981014      * AGGIORNO FNDCD Dettaglio C.A.
157200981014      *****************************************************************
157300981014     C     UPDDCD        BEGSR
157400981014     c*
157500981014     c     keynca        setll     fndcd01l
157600981014     c     keynca        reade     fndcd01l                               31
157700981014     c*
157800981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
157900981014     c                   dow       *in31 = *off
158000981014     c                   eval      DCddch = dctdch
158100981014     c                   eval      DCdcch = dctcch
158200981222     c                   eval      DCdftr = dctft1
158300981014     c                   update    fndcd000
158400981014     c*
158500981014     c     keynca        reade     fndcd01l                               31
158600981014     c*
158700981014     c                   enddo
158800981014     c*
158900981014     c                   endsr
159000980923      *****************************************************************
159100980923      * AGGIORNO FNDCF FASI C.A.
159200980923      *****************************************************************
159300980923     C     WRTDCF        BEGSR
159400980923      *
159500990226     c                   clear                   fndcf000
159600020201     c                   clear                   DDCF01
159700980923     C                   z-add     DCTaaC        DCFaac
159800980923     C                   z-add     DCTfil        DCFfil
159900980923     C                   z-add     DCTnca        DCFnca
160000981014     c                   z-add     dctfca        DCFfca
160100980923     c                   z-add     dateu         DCFdfc
160200980923     c                   z-add     wora          DCFhfc
160300980923     c                   z-add     V1Cfgs        DCFfev
160400981026     c                   movel     dctptr        DCFptr
160500980923     c                   movel     knmus         DCFpru
160600020201     c                   movel     §DCTpfra      §DCFpfra
160700020201     c                   movel     DDCF01        DCFlet
160800980923      *
160900980923     c                   WRITE     FNDCF000
161000980923      *
161100980923     c                   ENDSR
161200981105      *****************************************************************
161300981105      * AGGIORNO ARCHIVI   RIAPERTURA C.A. PER RIFIUTO TRANSAZIONE
161400981105      *****************************************************************
161500981105     C     SUB_RIFIU     BEGSR
161600990312      * disalloco il file fndct
161700990312     c                   unlock    fndct01l
161800981105      * Richiamo il pgm rifiuto transazione
161900981105      *
162000981105     c                   clear                   FIDN00DS
162100981105      *
162200981105     c                   movel     'G'           I00mod
162300981105     c                   movel     wrifi         I00fca
162400981105     c                   z-add     dctgfc        I00fgs
162500981105     c                   z-add     dctaac        I00aac
162600981105     c                   z-add     dctfil        I00fil
162700981105     c                   z-add     dctnca        I00nca
162800981105     c                   z-add     dctaas        I00aas
162900981105     c                   z-add     dctlnp        I00lnp
163000981105     c                   z-add     dctnrs        I00nrs
163100981105     c                   z-add     dctnsp        I00nsp
163200981105     c                   z-add     dctlna        I00lna
163300981105     c                   movel     dcttad        I00tad
163400981230     c                   if        dctgfc = W§dctlnpc
163500981105     c                   movel     'P'           I00tpb
163600981105     c                   endif
163700981105     c                   if        dctgfc = dctlna
163800981105     c                   movel     'A'           I00tpb
163900981105     c                   endif
164000981105     c                   if        dctgfc = 046
164100981105     c                   movel     'S'           I00tpb
164200981105     c                   endif
164300981105      *
164400981105     c                   movel     FIDN00DS      kpjbu
164500981105      *
164600981105     c                   call      'FIDN23R'
164700981105     c                   parm                    kpjba
164800990312     c                   movel     kpjbu         fidn00ds
164900990312     c*
165000990312      *
165100990312     C     Keynca        chain     FNDCT000                           31
165200990312      *
165300990312      * se non viene dato KL/KC  in FIDN23 aggiorno altrimenti riemetto la videata
165400990312     c                   if        o00f12 = ' ' and o00f03 = ' '
165500990312      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
165600990312     c                   z-add     *zeros        DCTdch
165700990312     c                   clear                   DCTcch
165800990312     c                   clear                   DCTft1
165900990312     c                   clear                   DCTft2
166000990312      *
166100990312      * Aggiorno testata C.A.
166200990312     c                   EXCEPT    UPDdct
166300990312      * Aggiorno l dettaglio C.A.
166400990312     c                   EXSR      UPDDCD
166500990312      *
166600990312     c                   endif
166700981105      *
166800981105     c                   ENDSR
166900050331      *****************************************************************
167000050331      *  SCRITTURA NOTE AGGIUTIVE
167100050331      *****************************************************************
167200050331     C     WRTDCS        BEGSR
167300050331      *
167400050331      * cerco l'ultimo progressivo note della fase in corso
167500050331      * - campi strandard
167600050331     c                   eval      Ktpd = 'C'
167700050331     c                   eval      aac = kaac
167800050331     c                   eval      fil = kfil
167900050331     c                   eval      nca = knca
168000050331     c                   eval      Knkt = dsnumca
168100050331     c                   eval      Kstd = ' '
168200050331     c                   eval      Kdim = dateu
168300050401     c                   eval      Khim = wora
168400050331      * - dati relativi alla CA in manutenzione
168500050331     c                   movel(p)  900           Knks
168600050331     c                   eval      Ktrc = 'N'
168700050331      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
168800050331     c     Kdcs          setgt     FNDCS01L
168900050331     c     Kdcs          readpe    FNDCS01L
169000050331      *
169100050331     c                   clear                   DCSatb
169200050331     c                   eval      DCStpd = Ktpd
169300050331     c                   eval      DCSnkt = Knkt
169400050331     c                   eval      DCSstd = Kstd
169500050331     c                   eval      DCSdim = Kdim
169600050331     c                   eval      DCShim = Khim
169700050331     c                   eval      DCSnks = Knks
169800050331     c                   eval      DCStrc = Ktrc
169900050331     c                   IF        %eof(FNDCS01L)
170000050331     c                   eval      DCSpno = 1
170100050331     c                   else
170200050331     c                   eval      DCSpno = DCSpno + 1
170300050331     c                   ENDIF
170400050401     c                   eval      DCSnot ='Il Cliente ha rifiutato l''assegno '
170500050331     c                   eval      DCSpru = KNMUS
170600050331     c                   eval      DCSpos = v1cfgs
170700050331     c                   clear                   DCSstn
170800050331     c                   clear                   DCSft1
170900050331     c                   clear                   DCSdt1
171000050331      *
171100050331     c                   write     FNDCS000
171200050401      * 2° riga
171300050401     c                   eval      DCSpno = DCSpno + 1
171400050401     c                   clear                   dcsnot
171500050401     c
171600050401     c                   eval      DCSnot = 'di traenza n°' + %trim(v1casn) +
171700050401     c                                      ' emesso il '
171800050401
171900050401     c                   write     FNDCS000
172000050401      * 3° riga
172100050401     c                   eval      DCSpno = DCSpno + 1
172200050401     c                   clear                   dcsnot
172300050401     c
172400050401     c                   eval      DCSnot = %editw(v1cdea:'  /  /    ') + ' di '
172500050401     c                             + %trim(%editc(v1cipl:'3')) + ' ' + v1cvpl
172600050331      *
172700050401
172800050401     c                   write     FNDCS000
172900050331     C                   ENDSR
173000120508      *****************************************************************
173100120508      *   REPERISCO NUMERO PRATICA ASSICURATIVA
173200120508      *****************************************************************
173300120508     C     NUMPRN        BEGSR
173400120508
173500120508      * verifico se CA legata ad evento
173600120508      * se legata verifico se nel file delle denucnce c'è il suo numero pratica
173700120508     c                   If        dctnev > 0
173800120508     c     K02DET01      chain     fndet01l
173900120508      * non trovo sul file denuncia vado via
174000120508     c                   If        not %found(fndet01l)
174100120508     c                   leavesr
174200120508     c                   endif
174300120508      * c'è il numero pratica imposto i campi e poi vado via
174400120508     c                   If        detprn > 0
174500120508     c                   unlock    fndet01l
174600120508     c                   eval      dctdit  =   DETdit
174700120508     c                   eval      dctpra  =   DETpra
174800120508     c                   eval      dctprn  =   DETprn
174900120508     c                   leavesr
175000120508     c                   endif
175100120508      * non c'è il numero pratica lo recupero
175200120508     c                   Clear                   FIDN20DS
175300120508     c                   Movel     'N'           I20mod
175400120508     c                   Z-add     DETaae        I20pra
175500120508     c                   Movel     DETdit        I20dit
175600120508     c                   movel(P)  FIDN20DS      KPJBU
175700120508     c                   CALL      'FIDN20R'
175800120508     c                   PARM                    KPJBA
175900120508     c                   movel     KPJBU         FIDN20DS
176000120508      * torna errore vado via
176100120508     c                   IF        O20err = 'E'
176200120508     c                   leavesr
176300120508     c                   ENDIF
176400120508      * c'è il numero pratica imposto i campi aggiorno il file denuncia
176500120508     c                   movel     I20dit        DETdit
176600120508     c                   z-add     I20pra        DETpra
176700120508     c                   z-add     O20prn        DETprn
176800120508     c                   update    fndet000
176900120508      * imposto i campi per la testata CA e vado via
177000120508     c                   eval      dctdit  =   DETdit
177100120508     c                   eval      dctpra  =   DETpra
177200120508     c                   eval      dctprn  =   DETprn
177300120508     c                   leavesr
177400120508     c                   endif
177500120508
177600120508      * La CA non è legata ad evento
177700120508      * recupero un numero pratica
177800120508     C                   clear                   FIDN20DS
177900120508     C                   movel     'N'           I20mod
178000120508     c                   z-add     DCTaas        I20pra
178100120508     c                   movel     'BAR'         I20dit
178200120508      * se la spedizione ha valore da assicurare esposto in bolla o
178300120508      *   assicurazione per conto: verifico se il P.O. di arrivo ha
178400120508      *   FEDEX come network
178500120508     c                   if        TASias > 0 and TASfcm <> 'F'
178600120508     c     DCTlna        Chain     AZORG
178700120508if  1c                   If            %found(AZORG01L)
178800120508     c                             and ORGfva = *blanks
178900120508     c                   Movel     ORGde3        OG143
179000120508if  2c                   If        §OGntw = 'FED'
179100120508     c                   Movel     §OGntw        I20dit
179200120508e   2c                   Endif
179300120508e   1c                   Endif
179400120508     c                   endif
179500120508     C                   movel(P)  FIDN20DS      KPJBU
179600120508     c                   CALL      'FIDN20R'
179700120508     c                   PARM                    KPJBA
179800120508     C                   movel     KPJBU         FIDN20DS
179900120508      * torna errore vado via
180000120508     c                   IF        O20err = 'E'
180100120508     c                   eval      *in28 = *ON
180200120508     c                   leavesr
180300120508     c                   ENDIF
180400120508      * imposto i campi per la testata CA
180500120508     c                   movel     I20dit        DCTdit
180600120508     c                   z-add     I20pra        DCTpra
180700120508     c                   z-add     O20prn        DCTprn
180800120508      *
180900120508     c                   ENDSR
181000980910      *****************************************************************
181100980527      * ROUTINE INIZIALE
181200980527      *****************************************************************
181300980527     C     *INZSR        BEGSR
181400980615      *
181500980521     C     *ENTRY        PLIST
181600980521     C                   PARM                    KPJBA
181700981014     c                   movel     kpjbu         fidn22ds
181800981014     c* se numero CA diversa da zeros  vuol dire che il programma è stato richiamato
181900981015     c                   if        i22tri = 'R'
182000981014     c                   eval      v1cfil = i22fil
182100981014     c                   eval      v1caac = i22aac
182200981014     c                   eval      v1cnca = i22nca
182300981021     c                   eval      v1ccch = i22cch
182400981202     c                   eval      v1cfgs = i22fgs
182500981021     c* decodifico causale chiusura se c'è
182600981021     c                   if        v1ccch <> '  '
182700981021     c                   clear                   dsbs02
182800981021     C                   MOVEL     'C'           t02mod
182900981021     C                   MOVEL     knsif         t02sif
183000981021     C                   MOVEL     'CCH'         t02cod
183100981021     C                   MOVEL(P)  v1ccch        t02ke1
183200981021      *
183300981021     C                   CALL      'TIBS02R'
183400981021     C                   PARM                    KPJBA
183500981021     C                   PARM                    dsbs02
183600981021      *
183700981021    4C                   if        t02err = *BLANKS
183800981021     C                   MOVEL     T02UNI        v1dcch
183900981021     c                   endif
184000990122     c*
184100990122     c                   else
184200010508      *
184300010508     C                   if        not %OPEN(Fidn22d)
184400990122     c                   open      fidn22d
184500010508     c                   endif
184600981021      *
184700981021     c                   endif
184800981021      *
184900981021     c* proteggo i campi del cambio p.o gestione e non abilito il CMD3
185000981021     c                   seton                                        060710
185100990122     c*
185200990122     c                   else
185300010508     C                   if        not %OPEN(Fidn22d)
185400010508     c                   open      fidn22d
185500010508     c                   endif
185600981014     c                   endif
185700980907      *
185800050929      * Reperisco dati job
185900050929     c                   exsr      DatiJob
186000050929     c                   eval      TBLkut = 1
186100020128      *
186200020128      * Reperisco la moneta corrente
186300020128     C                   clear                   DSBS02
186400020128     C                   movel     'L'           T02tla
186500020128     C                   movel     'C'           T02mod
186600020128     C                   movel     KNSIF         T02sif
186700020128     C                   movel     'GED'         T02cod
186800020128     C                   movel     'DANNI'       T02ke1
186900020128     C                   call      'TIBS02R'
187000020128     C                   parm                    KPJBA
187100020128     C                   parm                    DSBS02
187200020128     C                   If        T02err = *blanks
187300020128     C                   movel     T02UNI        DGEDDN
187400020128     C                   else
187500020128     C                   clear                   DGEDDN
187600020128     C                   Endif
187700020128      *
187800020128      * Reperisco tab.STD
187900020128     C                   clear                   DSBS02
188000020129     C                   movel     'L'           T02tla
188100020128     C                   movel     'C'           T02mod
188200020128     C                   movel     KNSIF         T02sif
188300020128     C                   movel     'STD'         T02cod
188400020128     C                   movel     '1'           T02ke1
188500020128     C                   movel     §gedDBA       T02ke2
188600020129     C                   call      'TIBS02R'
188700020129     C                   parm                    KPJBA
188800020129     C                   parm                    DSBS02
188900020128     C                   If        T02err = *blanks
189000020128     C                   movel     T02uni        DSTD
189100020128     C                   else
189200020128     C                   clear                   DSTD
189300020128     C                   Endif
189400980907
189500000228      * IMPOSTO IL P.O. IN GESTIONE se = a zeros
189600000228     c                   if        v1cfgs = *zeros
189700050929    2C                   IF           DUTlpo = '2'
189800050929     C                             or DUTlpo = *blanks
189900050929     C                   movel     DUTpou        V1CFGS
190000980521     C                   eval      *in06 = *on
190100980521   X2C                   ELSE
190200980521     C                   movel     SIMFEL        V1CFGS
190300980521    2C                   ENDIF
190400070404     c                   endif
190500981001      *
190600981001      * Se sede non visualizzo  "P.O. IN GESTIONE"
190700981001     C                   IF        V1Cfgs = *zeros
190800981001     C                   eval      *in06 = *on
190900981001     C                   eval      *in07 = *on
191000981001     C                   z-add     46            V1CFGS
191100170517     c                   eval      L6(1)=046
191200070404     c**!!!              ELSE
191300070404     c                   endif
191400070404      * Se non è sede carico £6 è altri dati
191500070404     c                   if        v1cfgs <> 46
191600070404     c                   clear                   og148
191700981012     C     V1CFGS        CHAIN     AZORG                              31
191800980521     C  N31              MOVEL     orgDES        V1DFGS
191900070404     c  n31              eval      og148 = orgde8
192000070404      * controllo se Filiale gestione è abilitata a telefonate GEO
192100070404     c                   eval      wfilgeo = (§oggeot = 'S')
192200170427
192300170427     c* richiamo controllo filiale gestine per impostare anche i flag abilitaz. part/arr
192400170427     c                   exsr      Ctrfgs
192500170427     c
192600170427     C***                exsr      CARL6
192700070404    2C**!!!              ENDIF
192800000228     c                   endif
192900981007      *
193000981007      * Imposto il TITOLO
193100981014      * chiusura
193200981014     c                   if        i22mod = 'C'
193300981014     c                   movel     TIT_chca      VTCtit
193400981130     c* valorizzo la fase di chiusura
193500981130     c                   eval      wfaca = wchca
193600981014     c                   endif
193700981014      * riapertura
193800981014     c                   if        i22mod = 'R'
193900981014     c                   movel     TIT_raca      vtctit
194000981130     c* pulisco la fase
194100981130     c                   clear                   wfaca
194200981014     c                   seton                                        08
194300981014     c                   endif
194400980721      * reperisco data e ora
194500980721     C                   TIME                    W0140
194600980521      * UDATE IN GGMMAAAA
194700980521     C                   MOVE      W0140         WDTGIO
194800980923     C                   MOVEL     W0140         WORA
194900980521      * UDATE IN AAAAMMGG
195000980521     C                   Z-ADD     WDTGIO        G02DAT
195100980521     C                   MOVEL     *BLANK        G02ERR
195200980521     C                   CALL      'XSRDA8'
195300980521     C                   PARM                    WLBDAT
195400980521     C                   MOVEL     G02INV        DATEU
195500980923
195600000117      *    Imposto anno C.A.  se non sono stato richiamato
195700000117     c                   if        i22tri <> 'R'
195800981013     c                   move      wdtgio        V1Caac
195900000117     c                   endif
196000050218
196100050218      * Aggancio tabella 5A rcd 2
196200050218     c                   Clear                   ds5a2
196300050218     c                   Eval      tblcod = '5A'
196400050218     c                   Eval      tblkey = '2       '
196500050218     c     ktab          Chain     Tabel00f
196600050218     c                   If        %Found(Tabel00f)
196700050218     c                   Eval      ds5a2 = TblUni
196800050218     c                   EndIf
196900980721
197000981014     C     Keynca        klist
197100981013     C                   KFLD                    kaac
197200981013     C                   KFLD                    kfil
197300981013     C                   KFLD                    knca
197400050805      * File Eventi
197500050805     c     K02DET01      klist
197600050805     c                   kfld                    DCTaae
197700050805     c                   kfld                    DCTnev
197800981016      * File giacenza
197900981016     c     kgiac         klist
198000040722     c                   kfld                    giaaas
198100040722     c                   kfld                    gialnp
198200040722     c                   kfld                    gianrs
198300040722     c                   kfld                    giansp
198400981016     c                   kfld                    Wfrg
198500981016      * File Bolle
198600981016     c     karb          klist
198700981016     c                   kfld                    dctaas
198800981016     c                   kfld                    dctlnp
198900981016     c                   kfld                    dctnrs
199000981016     c                   kfld                    dctnsp
199100010926      * file TITAS30C
199200010926     c     K04TAS30C     klist
199300010926     c                   kfld                    DCTaas
199400010926     c                   kfld                    DCTlnp
199500010926     c                   kfld                    DCTnrs
199600010926     c                   kfld                    DCTnsp
199700050218
199800050218     c     ktab          klist
199900050929     c                   kfld                    TBLkut
200000050218     c                   kfld                    tblcod
200100050218     c                   kfld                    tblkey
200200050331      * file FNDCS00F
200300050331     C     KDCS          KLIST
200400050331     C                   KFLD                    KTPD
200500050331     C                   KFLD                    KNKT
200600050331     C                   KFLD                    KSTD
200700050331     C                   KFLD                    KDIM
200800050331     C                   KFLD                    KHIM
200900050331     C                   KFLD                    KNKS
201000050331     C                   KFLD                    KTRC
201100980915
201200980521     C                   ENDSR
201300050929      *
201400050929      *---------------------------------------------------------------*
201500050929      * Reperimento Dati del job (Utente/Operativi)                   *
201600050929      *---------------------------------------------------------------*
201700050929     c     DatiJob       BEGSR
201800050929      *
201900050929     c     *dtaara       define    §azute        azuteds
202000050929     c     *dtaara       define    §datiute      ddatiute
202100050929      *
202200050929     c                   in(E)     *dtaara
202300050929     c                   IF        %ERROR or RSUT = *blanks
202400050929     c                   clear                   Tibs34Ds
202500050929     c                   call      'TIBS34R'
202600050929     c                   parm                    Tibs34Ds
202700050929     c                   in        *dtaara
202800050929     c                   ENDIF
202900050929      *
203000050929     c                   ENDSR
203100010926
203200010926      *****************************************************************
203300011008      * AGGANCIO FILE TITAS30C (PER CONTROLLO DATA BOLLA, MA NON SOLO)
203400010926      *****************************************************************
203500011008     c     ChaTITAS      BEGSR
203600010926      *
203700010926     c                   if        not %OPEN(TITAS30C)
203800010926     c                   open      TITAS30C
203900010926     c                   endif
204000010926      *
204100010926     c     K04TAS30C     chain     TITAS30C
204200011008     c                   if        not %found
204300011008     c                   clear                   TASaas
204400011008     c                   clear                   TASlnp
204500011008     c                   clear                   TASnrs
204600011008     c                   clear                   TASnsp
204700011008     c                   clear                   TASlna
204800010926     c                   endif
204900010926      *
205000010926     c                   ENDSR
205100010926
205200010926      *****************************************************************
205300090417      * AGGIORNAMENTO FILE FnRIG00f
205400010926      *****************************************************************
205500010926     c     WRTfirig      BEGSR
205600010926      *
205700090417     c                   if        not %OPEN(FnRIG00F)
205800090417     c                   open      FnRIG00F
205900010926     c                   endif
206000010926      *
206100090417     c                   clear                   FnRIG000
206200010926     c                   eval      RIGaas = TASaas
206300010926     c                   eval      RIGlnp = TASlnp
206400010926     c                   eval      RIGnrs = TASnrs
206500010926     c                   eval      RIGnsp = TASnsp
206600010926     c                   eval      RIGlna = TASlna
206700090417     c                   eval      RIGcau = 'D'
206800010926     c                   eval      RIGtra = 'E'
206900090417     c                   eval      RIGfil = V1Cfgs
207000010926     c                   eval      RIGpru = KNMUS
207100090417     c                   eval      RIGdata = Dateu
207200090417     c                   eval      RIGora = wora
207300010926      *
207400090417     c                   write     FnRIG000
207500010926      *
207600010926      * ...e rigenero la bolla legata alla C.A. riaperta
207700010926     c                   clear                   KPJBU
207800010926     c                   eval      kcoaz = 'BS21'
207900010926     c                   call      'BCH10'
208000010926     c                   parm                    kpjba
208100020128      *
208200010926     c                   ENDSR
208300080114      *
208400080114      *****************************************************************
208500080114      * Gestione videata D02
208600080114      *****************************************************************
208700080114     c     GesD02        BEGSR
208800080114      *
208900080114      * Inizializzazione videata
209000080114     c                   if        $InzD2 = *on
209100080114     c                   exsr      InzD02
209200080114     c                   eval      $InzD2 = *off
209300080114     c                   endif
209400080114      * Emissione videata
209500080114     c                   if        NOT *in90
209600080114     c                   write     FI22T01
209700080114     c                   endif
209800080114     c                   exfmt     FI22D02
209900080114     c                   setoff                                       28  90
210000080114     c                   clear                   V1Cmsg
210100080114      *
210200080114     c                   SELECT
210300080114      * F3=Fine
210400080114     c                   WHEN      *inKC
210500080114     c                   reset                   $Video
210600080114     c                   eval      O22f03 = 'S'
210700080114      * F12=Ritorno
210800080114     c                   WHEN      *inKL
210900080114     c                   reset                   $Video
211000080114      * F10=Note C.A.
211100080114     c                   WHEN      *inKJ
211200080114     c                   exsr      Sub_F10
211300080114      * F14=Interrogazione C.A.
211400080114     c                   WHEN      *inKN
211500080114     c                   exsr      Sub_F14
211600080114      * Invio / F6
211700080114     c                   OTHER
211800080114     c                   exsr      CtrD02
211900080115     c                   if        NOT *in90  and  *inKF
212000080115      * - Conferma delle eventuali note precedentemente inserite
212100080115     c                   clear                   dsDN10
212200080115     c                   eval      I10dta = WdataCA
212300080115     c                   eval      I10flm = 'C'
212400080115     c                   eval      I10tpd = 'C'
212500080115     c                   movel(p)  dsNumCA       I10nkt
212600080115     c                   movel(p)  WchCA         I10nks
212700080115     c                   eval      I10trc = 'N'
212800080115     c                   movel(p)  dsDN10        kpjbu
212900080115     c                   call      'FIDN10R'
213000080115     c                   parm                    kpjba
213100080115     c                   movel     kpjbu         dsDN10
213200080115      * - Note fase ca
213300080115     c                   exsr      Sub_F10
213400080115      * - Uscita
213500080115     c                   clear                   $Video
213600080115     c                   endif
213700080115     c                   ENDSL
213800080114      *
213900080114     c                   ENDSR
214000080114      *
214100080114      *****************************************************************
214200080114      * Inizializzazione videata D02
214300080114      *****************************************************************
214400080114     c     InzD02        BEGSR
214500080114      *
214600080114     c                   movea     *in           Sav_In
214700080114     c                   clear                   FI22D02
214800080114     c                   movea     Sav_In        *in
214900080114      *
215000080114      * - Riporto i dati da D01
215100080114     c                   eval      V2Cfgs = V1Cfgs
215200080114     c                   eval      V2Dfgs = V1Dfgs
215300080114     c                   eval      V2Cfil = V1Cfil
215400080114     c                   eval      V2Cnca = V1Cnca
215500080114     c                   eval      V2Caac = V1Caac
215600080114     c                   eval      V2Ccch = V1Ccch
215700080114     c                   eval      V2Dcch = V1Dcch
215800080114      *
215900080114      * - Importo massimo risarcibile
216000080114     c                   eval      V2Cipv = DCTipv
216100080114     c                   eval      V2Cvpv = DCTvpv
216200080123      * - Tipo Calcolo
216300080123     c                   select
216400080123     c                   when      DCTfpv = 'M'
216500080123     c                   eval      V2Dfpv = 'Inserito manualmente'
216600080123     c                   when      DCTfpv = 'C'
216700080123     c                   eval      V2Dfpv = 'Calcolato'
216800080123     c                   endsl
216900080114      *
217000080114      * Reperimento "importo richiesto" ed "importo liquidato"
217100080114if  1c                   if        NOT %open(FNDCL01L)
217200080114     c                   open      FNDCL01L
217300080114e   1c                   endif
217400080114     c     KEYnca        chain(n)  FNDCL000
217500080114if  1c                   IF        %found(FNDCL01L)
217600080114      * - Importo richiesto
217700080114     c                   eval      V2Cipr = DCLipr
217800080114     c                   eval      V2Cvpr = DCLvpr
217900080114      * - Importo liquidato
218000080114     c                   eval      V2Cipl = DClipl
218100080115     c                   eval      V2Cvpl = DCLvpl
218200080114e   1c                   ELSE
218300080114     c                   clear                   V2Cipr
218400080114     c                   clear                   V2Cvpr
218500080114     c                   clear                   V2Cipl
218600080114e   1c                   ENDIF
218700080115     c                   if        V2Cvpl = *blanks
218800080115     c                   eval      V2Cvpl = §gedDBA
218900080115     c                   endif
219000080114if  1c                   if        %open(FNDCL01L)
219100080114     c                   close     FNDCL01L
219200080114e   1c                   endif
219300080114      *
219400080114     c                   ENDSR
219500080114      *
219600080114      *****************************************************************
219700080114      * Controllo dati videata D02
219800080114      *****************************************************************
219900080114     c     CtrD02        BEGSR
220000080114      *
220100080114     c                   movea     *zeros        *in(40)
220200080115      *
220300080115     c                   if        V2Cipl < *zeros
220400080115     c                   seton                                        284390
220500080115     c                   eval      V1Cmsg = Msg(22)
220600080115     c                   leavesr
220700080115     c                   endif
220800080114      *
220900080114     c                   ENDSR
221000080114
221100980521      *---------------------------------------------------------------------------------------------
221200981007     oFNDCT000  E            UPDDCT
221300981013     O                       DCTdch
221400981013     O                       DCTcch
221500981013     O                       DCTfca
221600080701     O                       DCTgfc
221700020226     O                       DCTpgd
221800120507     O                       DCTksc
221900120508     o                       DCTDIT
222000120508     o                       DCTPRA
222100120508     o                       DCTPRN
222200990430     O                       DCTflo
222300981222     O                       DCTft1
222400981222     O                       DCTft2
222500050408     O                       DCTdt2
222600980923      *---------------------------------------------------------------------------------------------
222700980521** MSG  Lungh. 78                                                            *
222800981013Il numero C.A. immesso non esiste
222900981014Il valore immesso nel campo non è valido                                       2
223000981014Causale di chiusura errata                                                     3
223100170427C.A. in gestione ad altra filiale                                              4
223200981014C.A. già chiusa                                                                5
223300981014C.A. non chiusa impossibile riaprirla                                          6
223400981021Impossibile riaprire C.A. : pratica giacenza aperta                            7
223500010926La C.A. non si può più riaprire perchè la bolla è antecedente il 01/09/1999    8
223600981021Impossibile riaprire C.A. : spedizione con blocco                              9
223700990311Non si può usare questa causale per chiusure non automatiche                  10
223800990311C.A. in fase di contabilizzazione impossibile riaprirla                       11
223900990311C.A. in fase di pagamento per Pratica Assicurativa impossibile riaprirla      12
224000990311La C.A. non si può più riaprire per mancanza dettaglio colli                  13
224100050218La C.A. non si può più riaprire perchè decorsi xxx gg. da data chiusura       14
224200050126Non si può usare questa causale se non stampato progetto liquidazione         15
224300050126Causale di chiusura che può usare solo la sede                                16
224400050331Utente non autorizzato ad aprire questa C.A.                                  17
224500050512Causale Chiusura utilizzabile per spedizioni di data &PRE/SUC      17/04/2005 18
224600131011La C.A. non si può riaprire. Avviate richieste di recupero ad A.I.G.          19
224700131011Causale di Pagamento utilizzabile solo per C.A. NON affidata ad A.I.G.        20
224800050929Causale di chiusura utilizzabile solo da utenti di                            21
224900080115L'importo liquidato NON può essere negativo                                   22
