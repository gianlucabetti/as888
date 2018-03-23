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
049800170427     c  n32w§DCTlnpc     lookup    L6                                     32
049900170427    5c                   if        *in32 and d50val_p='N'
050000170427     c                   setoff                                       32
050100170427    5c                   endif
050200170427    5c                   if        not *in32
050300170427     c                   setoff                                       34
050400170427    5c                   endif
050500170427    4c                   endif
050600170427     c
050700170427    3c***                if        v1cfgs <> dctgfc
050800981014     C* verifico se si trova nella sua £6
050900170427     c***  dctgfc        lookup    L6                                     30
051000170427     c**n30              seton                                        2841
051100170427     c  n34              seton                                        2841
051200990712     C   28              movel     MSG(4)        V1Cmsg
051300990712     c   28              goto      Ectrd01
051400170427    3c*****              ENDIF
051500981030     c                   endif
051600050512     c*
051700050512    2c                   ENDIF
051800050512    1C                   ENDIF
051900160913      *
052000160913      /free
052100160913
052200160913         // -?Eventuale visualizzazione estremi della Pratica?
052300160913         if  V1Ccch <> WchBB  or
052400160921             DCTfca <  wPrLi  or
052500160921             *in28  =  *on;
052600160913           leavesr;
052700160913         endif;
052800160913
052900160913         *in23 = *on;
053000160914
053100160914         // -?Pratica affidata ad A.I.G.?
053200160914         *in71 = (§DCTaffi = 'A');
053300160914         // -?Pratica Assicurativa?
053400160914         *in72 = (DCTfpr  <> 'T'  and  §DCTpFra <> 'F');
053500160914         // -?Liquidazione Transattiva?
053600160914         *in73 = (DCTfpr   = 'T'  and  §DCTpFra <> 'F');
053700160914         // -?Pratica in Franchigia?
053800160915         *in74 = (§DCTpFra = 'F');
053900160921
054000160921         // -?Eventuale visualizzazione ALERT x anno trascorso dalla fase 700?
054100160921         If  V1Ccch = WchBB  and  DCTfca = wPrLi;
054200160921
054300160921           setGT     ( Kaac : Kfil : Knca )  FNDCF000;
054400160921           readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
054500160921           DoW  NOT %eof( FNDCF01L)  and  DCFfca <> wPrLi;
054600160921             readPE(N) ( Kaac : Kfil : Knca )  FNDCF000;
054700160921           EndDo;
054800160921
054900160921           if  NOT %eof( FNDCF01L )  and  DCFfca = DCTfca;
055000160921             *in24 = ( DCFdfc <= %dec( %date() - %years(1) ) );
055100160921           endif;
055200160921
055300160921         EndIf;
055400160921
055500160921         // -?Eventuale visualizzazione dati della Chiusura?
055600160921         If  DCTfca = wChCA;
055700160921
055800160921           *in70 = *on;
055900160921
056000160921           if  DCTdch > *zero;
056100160921             WdataEUR = %date( DCTdch : *iso );
056200160921             V1Odch   = %dec( WdataEUR );
056300160921           endif;
056400160921
056500160921           if  DCTcch > *blank;
056600160921             V1Occh = DCTcch;
056700160921             clear  dsBS02;
056800160921             T02mod = 'C';
056900160921             T02cod = 'CCH';
057000160921             T02ke1 = DCTcch;
057100160921      /end-free
057200160921     c                   call      'TIBS02R'
057300160921     c                   parm                    KPJBA
057400160921     c                   parm                    dsBS02
057500160921      /free
057600160921             if  T02err = *blank;
057700160921               dCCH    = T02uni;
057800160921               V1OcchD = §CCHdesc;
057900160921             endif;
058000160921           endif;
058100160921           // -?Verifica se è già stata fatta almeno un'estrazione?
058200160921           *in75 = ( §DCTpaga = 'C' );
058300160921           // -?Verifica se è già stato fatto il pagamento?
058400160921           *in76 = ( §DCTpaga = 'P' );
058500160921
058600160921         EndIf;
058700160915
058800160915         if  Not *in71  and  Not *in72  and
058900160915             Not *in73  and  Not *in74;
059000160915           *in23 = *off;
059100160921           *in24 = *off;
059200160915           leavesr;
059300160915         endif;
059400160913
059500160914         // -?Beneficiario pagamento C.A.?
059600160914         //  ?(SE Liquidazione Transattiva o Pratica in Franchigia)?
059700160914         if  *in73  or  *in74;
059800160914           chain(N)  ( Kaac : Kfil : Knca : 'P' )  FNDKA000;
059900160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
060000160914             V1Cpli = DKAksc;
060100160914             V1Dpli = DKArag;
060200160914           endif;
060300160914         // -?Intestatario Progetto Liquidazione?
060400160914         else;
060500160914           chain(N)  ( Kaac : Kfil : Knca : 'I' )  FNDKA000;
060600160914           if  %found(FNDKA01L)  and  DKAatb <> 'A';
060700160914             V1Cpli = DKAksc;
060800160914             V1Dpli = DKArag;
060900160914           endif;
061000160914         endif;
061100160913
061200160913
061300160921         // -?Altri dati del Progetto di Liquidazione:?
061400160913         if  Not %open(FNDCL01L);
061500160913           open  FNDCL01L;
061600160913         endif;
061700160913         chain(N)  ( Kaac : Kfil : Knca )  FNDCL000;
061800160915
061900160915         If  %found(FNDCL01L);
062000160913
062100160915           // -?Data Stampa Progetto di Liquidazione?
062200160915           if  DCLdds > *zeros;
062300160915             WdataEUR = %date( DCLdds : *iso );
062400160915             V1Cdds   = %dec( WdataEUR );
062500160915           endif;
062600160915
062700160915           // -?Data Accettazione Progetto di Liquidazione?
062800160915           if  DCLdpl > *zeros;
062900160915             WdataEUR = %date( DCLdpl : *iso );
063000160915             V1Cdpl   = %dec( WdataEUR );
063100160915           endif;
063200160915
063300160915           // -?Importo Liquidato?
063400160915           if  DCTfpr = 'T';
063500160915             //*·V2Cipl = DCLipl;
063600160915             //*·V2Cvpl = DCLvpl;
063700160915           else;
063800160915             V1Cipp = DCLipl;
063900160915             V1Cvpp = DCLvpl;
064000160915             //*·// -?Se pratica trattenuta da cliente e?
064100160915             //*·//     ?pagamento già avvenuto?
064200160915             //*·//  ?=> imposto come importo liquidato la differenza?
064300160915             //*·if  DCLipl <= DCLipt;
064400160915             //*·  clear  V2Cipl;
064500160915             //*·  clear  V2Cvpl;
064600160915             //*·else;
064700160915             //*·  V2Cipl = DCLipl - DCLipt;
064800160915             //*·  V2Cvpl = DCLvpl;
064900160915             //*·endif;
065000160915           endif;
065100160915
065200160915           // -?Data valuta pagamento?
065300160915           if  DCLvlp > *zeros;
065400160915             WdataEUR = %date( DCLvlp : *iso );
065500160915             V1Cvlp   = %dec( WdataEUR );
065600160915           endif;
065700160915
065800160915         EndIf;
065900160913
066000160913      /end-free
066100050512
066200050512     c     Ectrd01       ENDSR
066300050512      **********************************************************************
066400050512      * Altri CONTROLLI VIDEO 1
066500050512      **********************************************************************
066600050512     C     CtrD01_b      BEGSR
066700020128      *
066800020128      * Verifico se C.A. con flag di franchigia = 'F'
066900020128      *                  e  causale di chiusura = '15'
067000020128     c                   if            §DCTpfra = 'F'
067100020226     c                             and DCTcch   = WchFR
067200020128     c                   eval      $Franch = *on
067300020128     c                   else
067400020128     c                   eval      $Franch = *off
067500020128     c                   endif
067600981016     c*
067700981016    3c                   if        *in08 = *off
067800981016     c* controllo nel caso di chiusura CA
067900981016     c                   exsr      SUB_ctrch
068000981016   x3c                   else
068100981016     c* controllo nel caso di riapertura CA
068200981016     c                   exsr      SUB_ctrri
068300981016    3c                   endif
068400050512     c   28              goto      Ectrd01b
068500980923
068600050512     c     Ectrd01b      ENDSR
068700981016      ****************************************************************
068800981016      * SUB_CTRCH Controlli di chiusura CA
068900981016      ****************************************************************
069000981016     C     SUB_CTRCH     BEGSR
069100981016      *
069200981016     c* controllo se C.A. già chiusa
069300981016    1c                   if        dctdch > 0
069400981016     c                   seton                                        2841
069500981016     C                   movel     MSG(5)        V1Cmsg
069600981016    1c                   ENDIF
069700981016      *
069800981016      * Controllo la causale di chiususra
069900981016    1c                   if        *in28 = *off
070000981030    2c                   if        v1ccch = '  '
070100981016     c                   seton                                        4228
070200981016     c                   movel     msg(3)        v1cmsg
070300981016     c                   goto      ech01
070400981016    2c                   endif
070500050126      * solo la sede
070600050511    2c                   if           (V1Ccch = WchBA
070700050511     c                             or  V1Ccch = WchBB)
070800050511     c                             and *in07  = *off
070900050126     c                   seton                                        4228
071000050126     c                   movel     msg(16)       v1cmsg
071100050126     c                   goto      ech01
071200050126    2c                   endif
071300050512      *
071400050512      * SOLO IN SEDE controllo la causale di spedizione 17/18
071500050512  1.5c                   if        *in07
071600050511      * reperisco la data spedizione
071700050511     c                   exsr      ChaTITAS
071800050511      * causale chiusura 17 errata se data spedizione >  17/04/2005
071900050511    2c                   if            (DCTaas * 10000 + TASmgs)
072000050511     c                              >  C_DataChiusLim
072100050511     c                             and V1Ccch = WchBA
072200050511     c                   seton                                        2842
072300050512     c                   eval      V1Cmsg = %replace('precedente il' :
072400050511     c                                      MSG(18) :
072500050512     c                                      %scan('&PRE/SUC':MSG(18)))
072600050511     c                   goto      ech01
072700050511    2c                   endif
072800050511      * causale chiusura 18 errata se data spedizione <= 17/04/2005
072900050511    2c                   if            (DCTaas * 10000 + TASmgs)
073000050511     c                              <= C_DataChiusLim
073100050511     c                             and V1Ccch = WchBB
073200050511     c                   seton                                        2842
073300050512     c                   eval      V1Cmsg = %replace('successiva al' :
073400050511     c                                      MSG(18) :
073500050512     c                                      %scan('&PRE/SUC':MSG(18)))
073600050511     c                   goto      ech01
073700050511    2c                   endif
073800050512  1.5c                   endif
073900050512      *
074000981016      * controllo la causale di chiusura
074100981016    2c                   if        v1ccch <> '  '
074200981016      *
074300981016     C     '?'           SCAN      v1Ccch                                 31
074400981016      *
074500981016    3C                   IF        *IN31 = *ON
074600990419     c                   seton                                        90
074700981016     c                   clear                   dsbs02
074800981016     c                   movel     'R'           t02mod
074900981016     c                   movel     knsif         t02sif
075000981016     C                   movel     'CCH'         t02cod
075100981016      *
075200981016     C                   CALL      'TIBS02R'
075300981016     c                   parm                    KPJBA
075400981016     c                   parm                    dsbs02
075500981016      *
075600981016    4c                   if        t02err =  *blanks
075700981016     C                   MOVEL     t02ke1        v1ccch
075800981016    5C                   IF        v1ccch <> *BLANKS
075900981016     c                   movel     t02uni        v1dcch
076000990311     c                   clear                   dcch
076100990311     C                   MOVEL     T02UNI        dcch
076200990311     c* verifico se causale si può usare a video
076300990311     c                   if        §cchgevi <> 'S'
076400990311     c                   movel     msg(10)       v1cmsg
076500990311     c                   seton                                        4228
076600990311     c                   goto      ech01
076700990311     c                   endif
076800990311     c*
076900981016    5C                   ENDIF
077000981030     c                   else
077100981030     c                   movel     t02msg        v1cmsg
077200981030     c                   goto      ech01
077300981016    4c                   endif
077400981016      *
077500981016     C                   GOTO      Ech01
077600981016   x3C                   Else
077700981016      *   Controlli di validità'
077800981016      *
077900981016     c                   clear                   dsbs02
078000981016     C                   MOVEL     'C'           t02mod
078100981016     C                   MOVEL     knsif         t02sif
078200981016     C                   MOVEL     'CCH'         t02cod
078300981016     C                   MOVEL(P)  v1ccch        t02ke1
078400981016      *
078500981016     C                   CALL      'TIBS02R'
078600981016     C                   PARM                    KPJBA
078700981016     C                   PARM                    dsbs02
078800981016      *
078900981016    4C                   if        t02err = *BLANKS
079000981016     C                   MOVEL     T02UNI        v1dcch
079100990311     c                   clear                   dcch
079200990311     C                   MOVEL     T02UNI        dcch
079300990311     c* verifico se causale si può usare a video
079400990311     c                   if        §cchgevi <> 'S'
079500990311     c                   movel     msg(10)       v1cmsg
079600990311     c                   seton                                        4228
079700990311     c                   goto      ech01
079800990311     c                   endif
079900990311     c*
080000981016     c                   else
080100990311     c*
080200981016     c                   movel     msg(3)        v1cmsg
080300981016     c                   seton                                        4228
080400981016     c                   goto      ech01
080500981016    4C                   endif
080600981016    3C                   endif
080700981016     c                   else
080800981016     c                   clear                   v1dcch
080900981016    2C                   endif
081000981016    1C                   endif
081100050126     c* controllo se C.A. in fase minore del progetto di liquidazione
081200050511    1c  n28              if             DCTfca < Wprli
081300050511     c                             and (V1Ccch = WchBA
081400050511     c                              or  V1Ccch = WchBB)
081500050126     c                   seton                                        2842
081600050126     C                   movel     MSG(15)       V1Cmsg
081700050929     c                   leavesr
081800050126    1c                   ENDIF
081900050929      *
082000050929      * controlli sull'utilizzo della causale in base ai flag in DCCH:
082100050929      * - per le causali di pagamento: ci deve essere compatibilità
082200131011      *   con l'affidamento (o meno) ad A.I.G. già memorizzato su
082300050929      *   FNDCT00F (in DCTFLO - ds DDCT01)
082400111223if  1c****               if            §CCHpaga  = 'S'
082500111223     c****                         and §DCTaffi  = 'A'
082600111223     c****                         and V1Ccch   <> WchBB
082700111223     c****               eval      V1Cmsg   = %trim(Msg(20))
082800111223     c****               seton                                        2842
082900111223     c****               leavesr
083000111223e   1c****               endif
083100050929      * - per le causali utilizzabili solo in sede: verifico la
083200050929      *   compatibilità con il profilo utente
083300051025sel 1c                   select
083400050929      * - - utilizzabile da tutti gli utenti
083500051025w   1c                   when      §CCHutil = *blanks
083600050929      * - - utilizzabile dai soli utenti di sede
083700051025w   1c                   when          §CCHutil =  'S'
083800050929     c                             and DUTlpo   <> 'S'
083900050929     c                   eval      V1Cmsg = %trim(Msg(21))
084000050929     c                                    + ' sede'
084100050929     c                   seton                                        2842
084200050929     c                   leavesr
084300050929      * - - utilizzabile dai soli utenti di p.o.
084400051025w   1c                   when          §CCHutil =  'P'
084500050929     c                             and DUTlpo   =  'S'
084600050929     c                   eval      V1Cmsg   = %trim(Msg(21))
084700080114     c                                      + ' filiale'
084800050929     c                   seton                                        2842
084900050929     c                   leavesr
085000051025e   1c                   endsl
085100981130     c*
085200981016
085300981016     c     ech01         ENDSR
085400981016      ****************************************************************
085500981016      * SUB_CTRRI Controlli di riapertura CA
085600981016      ****************************************************************
085700981016     C     SUB_CTRRI     BEGSR
085800981016      *
085900981016     c* controllo se C.A. non chiusa  in caso di riapertura C.A.
086000981130    1c                   if        dctdch = 0
086100981016     c                   seton                                        2841
086200981016     C                   movel     MSG(6)        V1Cmsg
086300981016     c                   goto      eri01
086400981016    1c                   ENDIF
086500050922      *
086600131011     c* controllo se C.A. già in fase di richiesta recupero ad A.I.G.
086700050922     C     Keynca        chain     FNDRA01L
086800050922    1c                   if        %found(fndra01l) and dradrr > 0
086900050922     c                   seton                                        2841
087000050922     C                   movel     MSG(19)       V1Cmsg
087100050922     c                   goto      eri01
087200050922    1c                   ENDIF
087300990712     c*
087400990712     c* calcolo la data massima per riapertura CA
087500990712     c     *iso          move      dctdch        wdata
087600050218     c**!!!              adddur    4:*m          wdata
087700050218     c                   adddur    §5abca:*d     wdata
087800990712     c     *iso          move      wdata         wrkdta
087900010926     c*
088000010926     c* se non riapribile in modo normale:
088100050218     c* solo in sede: riapertura consentita oltre i giorni da tabella 5A
088200010926     c*           ma  non se bolla antecedente il 1° settembre 1999.
088300010926     c* (modifico la data massima per riapertura CA - wrkdta)
088400010926     c                   eval      $over4m = *off
088500011008    1c                   if        *in07         and
088600010926     c                             dateu   > wrkdta
088700010926     c                   eval      $over4m = *on
088800011008     c                   exsr      ChaTITAS
088900011008     c* ok oltre il 01/09/1999
089000011008    2c                   if         DCTaas  > 1999   or
089100011008     c                             (DCTaas  = 1999  and  TASmgs >= 0901)
089200011008     c                   eval      wrkdta  = *hival
089300011008     c                   else
089400011008     c                   seton                                        2841
089500011008     c                   movel     MSG(8)        V1Cmsg
089600011008     c                   goto      eri01
089700011008    2c                   endif
089800011008    1c                   endif
089900010926     c*
090000050218     c* controllo se CA chiusa da più giorni da tab. 5A
090100990712     c                   if        dateu > wrkdta
090200990712     c                   seton                                        2841
090300050218     c                   move      §5abca        w003a             3
090400050218     C                   movel     MSG(14)       V1Cmsg
090500050218     c                   Eval      %subst(v1cmsg:48:3) = w003a
090600990712     c                   goto      eri01
090700990712    1c                   endif
090800990712     c
090900990311     c* controllo se la C.A. chiusa  non sia in fase di contabilizzazione
091000990311    1c                   if        §dctpaga = 'C'
091100990311     c                   seton                                        2841
091200990311     C                   movel     MSG(11)       V1Cmsg
091300990311     c                   goto      eri01
091400990311    1c                   endif
091500050331      * verifico se la CA chiusa può essere riaperta solo da un utente autorizzato
091600050331      *
091700050331     c                   clear                   dsbs02
091800050331     C                   MOVEL     'C'           t02mod
091900050331     C                   MOVEL     knsif         t02sif
092000050331     C                   MOVEL     'CCH'         t02cod
092100050331     C                   MOVEL(P)  dctcch        t02ke1
092200050331      *
092300050331     C                   CALL      'TIBS02R'
092400050331     C                   PARM                    KPJBA
092500050331     C                   PARM                    dsbs02
092600050331      *
092700050331     c                   clear                   dcch
092800050331     C                   MOVEL     T02UNI        dcch
092900050331      * se richiesto il controllo dell'utente escludo gli utenti EDP
093000050331     c                   If        §cchprot = 'S' and %subst(knmus:1:3)<>'EDP'
093100050331      *
093200160914     c     knmus         lookup    ute                                    33
093300050331     c                   if        not %found
093400050331     c                   seton                                        2841
093500050331     C                   movel     MSG(17)       V1Cmsg
093600050331     c                   goto      eri01
093700050331    1c                   endif
093800050331    1c                   endif
093900990311     c* controllo se C.A. in fase di pagamento non deve essere per pratica assicurativa
094000050420    1c                   if        §dctpaga = 'P'  and
094100050331     c                             (dctcch = wchpa )
094200050420     c*                  seton                                        2841
094300050420     C*                  movel     MSG(12)       V1Cmsg
094400050420     c*                  goto      eri01
094500990311    1c                   endif
094600990311     c* Controllo se il numero dei colli in testata CA è uguale al numero dei colli nel dettaglio
094700990311     c                   exsr      SUB_COLLI
094800990311     c*
094900990311     c* se numero colli differenti non si può aprire
095000990311    1c                   if        dctncn <> wnco
095100990311     c                   seton                                        2841
095200990311     C                   movel     MSG(13)       V1Cmsg
095300990311     c                   goto      eri01
095400990311    1c                   endif
095500981016     c*
095600981016     c* controllo chi sono io
095700981230     c                   if        dctgfc = W§dctlnpc
095800981016     c                   movel     'P'           chisono                        * PARTENZA
095900040722     c                   eval      giaaas = dctaas
096000040722     c                   eval      gialnp = dctlnp
096100040722     c                   eval      gianrs = dctnrs
096200040722     c                   eval      giansp = dctnsp
096300981016     c                   endif
096400981016     c                   if        dctgfc = dctlna
096500981016     c                   movel     'A'           chisono                        * ARRIVO
096600040722      * verifico se ho spedizione figlia su FNDCK prendo quella
096700040722     c     keynca        chain     fndck01l
096800040722     c                   if        %found(fndck01l)
096900040722     c                   eval      giaaas = dckaas
097000040722     c                   eval      gialnp = dcklnp
097100040722     c                   eval      gianrs = dcknrs
097200040722     c                   eval      giansp = dcknsp
097300040722     c                   else
097400040722     c                   eval      giaaas = dctaas
097500040722     c                   eval      gialnp = dctlnp
097600040722     c                   eval      gianrs = dctnrs
097700040722     c                   eval      giansp = dctnsp
097800040723     c                   endif
097900040723      *
098000981016     c                   endif
098100981016     c                   if        dctgfc = 046
098200981016     c                   movel     'S'           chisono                        * SEDE
098300981016     c                   endif
098400981016     c*
098500040722     c* sono l' ARRIVO o la PARTENZA controllo la giacenza in ARRIVO
098600040722     c                   if        chisono <> 'S'
098700010508      *
098800050404     C                   if        not %OPEN(Tigcp21l)
098900050404     C                   OPEN      Tigcp21l
099000010508     c                   endif
099100010508      *
099200050404     C     kgiac         chain     Tigcp21l                           32
099300050404     c                   if        not *in32 and gcpfas < 40
099400981016     c                   seton                                        2841
099500981021     C                   movel     MSG(7)        V1Cmsg
099600981016     c                   goto      eri01
099700981016     c                   endif
099800010508      *
099900050404     C                   if        %OPEN(Tigcp21L)
100000050404     c                   close     Tigcp21l
100100010508     c                   endif
100200010508      *
100300010508      * controllo la data consegna e il numero Distinta consegna
100400010508     C                   if        not %OPEN(FNARB01L)
100500010508     C                   OPEN      FNARB01L
100600010508     c                   endif
100700010508      *
100800990311     C     karb          chain     fnarb01l                           32
100900981230     c* controllo se c'è un flag blocco di spedizione diverso da "D", da " " e da "*"
101000981230     c                   if        arbfbc <> *blanks  and  arbfbc <> '*'
101100981021     c                   movel     msg(9)        V1Cmsg
101200981019     c                   seton                                        2841
101300981019     c                   goto      eri01
101400981019     c                   endif
101500010508      *
101600010508     C                   if        %OPEN(FNARB01L)
101700010508     C                   close     FNarb01L
101800010508     c                   endif
101900010508      *
102000981016     c                   endif
102100981016     c*
102200981019      * SE NON CI SONO ERRORI
102300981019     c* cerco la fase precedente la chiusura CA
102400981202     c     keynca        setgt     fndcf01l
102500981019     c     keynca        readpe    fndcf01l                               31
102600981202     c                   dow       dcffca = wchca
102700981019     c     keynca        readpe    fndcf01l                               31
102800981202     c                   enddo
102900981130     c  n31              eval      wfaca = dcffca
103000981105     c*
103100981105     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
103200990511     c* liquidazione transattiva : se è così imposto se si tratta di un rifiuto assegno di traenza
103300990311     c                   if        chisono  = 'S'  and  §dctpaga = 'P' and
103400990511     c                             dcffca = wcolt  and  *in20 = *off
103500981105     c                   seton                                        2021
103600981105     c                   endif
103700990430     c*
103800990430     c* se sono la SEDE controllo se la fase precedente la chiusura è la fase di contabilizzazione
103900090925     c* liquidazione transattiva : se è così non imposto + la fase di esito ma quella di av.danno
104000090925     c* liquidazione transattiva
104100990430     c                   if        chisono  = 'S'  and  §dctpaga = ' ' and
104200990511     c                             dcffca = wcolt  and  *in20 = *off
104300090925     c*******            eval      wfaca  = wesit
104400090925     c                   eval      wfaca  = wavdt
104500990430     c                   endif
104600050331      * se sto riaprendo una CA pagata in surroga visualizzo i dati dell'assegno
104700050511     c                   if            (DCTcch   = WchBA
104800051025     c                              or  DCTcch   = WchBB)
104900050511     c                             and  §DCTpaga = 'P'
105000050511     c                             and  *in20    = *off
105100050331     c                   seton                                        202122
105200050331     c                   endif
105300050401      * per 21 acceso visualizzo gli estremi del pagamento
105400050401      *
105500050401     c                   If        *in21
105600050401
105700050401     C                   if        not %OPEN(Fndcl01L)
105800050401     c                   open      fndcl01l
105900050401     c                   endif
106000050401      *
106100050401     c     keynca        chain(n)  fndcl01l
106200050401     c                   If        %found(fndcl01l)
106300050401      * Data emissione Assegno
106400050401     c                   if        DCLdea > *zeros
106500050401     c     *iso          move      DCLdea        Wdataeur
106600050401     c     *eur          movel     Wdataeur      V1Cdea
106700050401     c                   endif
106800050401      *
106900050401      * Numero  Assegno
107000161206      * o Bonifico
107100050401     c                   movel     dclasn        v1casn
107200161206     c                   clear                   V1Dasn
107300161206     c                   SELECT
107400161206     c                   WHEN      dclasn = *blanks
107500161206     c                   eval      V1Dasn = 'Ass/Bon'
107600161206     c                   WHEN      %subst(dclasn:1:1) = 'B'
107700161206     c                   eval      V1Dasn = 'Bonifico'
107800161206     c                   OTHER
107900161206     c                   eval      V1Dasn = 'Assegno'
108000161206     c                   ENDSL
108100050401      * importo liquidato
108200050401     c                   z-add     DClipl        V1Cipl
108300050401     c                   movel     dclvpl        v1cvpl
108400050401
108500050401     c                   endif
108600050401
108700050401     c                   if        %OPEN(Fndcl01L)
108800050401     c                   close     fndcl01l
108900050401     c                   endif
109000050401
109100050401     c                   endif
109200120507
109300120507      *?Controllo se si sta riaprendo un Porto Franco Import EEX chiuso con CAU 06
109400120507     c                   eval      wPFIEEX = *off
109500120507     c                   IF        §DCTport = 'F' and §DCTtisp = 'I' and
109600120507     c                              DCTcch = wCHEE
109700120507     c                   eval      wPFIEEX = *on
109800120507      *?Recupero la data della bolla
109900120507     c                   exsr      ChaTITAS
110000120507     c                   eval      Wdaspe = (TASaas * 10000) + TASmgs
110100120507     c                   ENDIF
110200981016
110300981016     c     eri01         ENDSR
110400981014      ****************************************************************
110500981014      * F10 = GESTIONE NOTE
110600981014      ****************************************************************
110700981014     C     SUB_F10       BEGSR
110800981014      *
110900981015     c*
111000981015     c                   eval      aac = kaac
111100981015     c                   eval      fil = kfil
111200981015     c                   eval      nca = knca
111300981015     c*
111400981014     C                   clear                   DSDN10
111500981015      * calcolo data apertura C.A.
111600981015     C                   eval      wdataca = (DCTaac * 10000) + DCTmgc
111700981015     c*
111800981015     C                   CLEAR                   WLBDAT
111900981015     C                   Z-ADD     wdataca       G02inv
112000981015     C                   movel     '3'           G02err
112100981015     C                   CALL      'XSRDA8'
112200981015     C                   PARM                    WLBDAT
112300981015     c                   z-add     g02dat        wdataca
112400981015     c*
112500981015     c                   eval      i10dta = wdataca
112600981014     c                   movel     'N'           i10trc
112700981014     c                   movel     'C'           i10tpd
112800981014      *
112900080115     c                   if        $Video = 'D2'  and  *inKF
113000080115     c                   eval      I10flm = 'V'
113100080115     c                   else
113200981015     c                   movel     'M'           i10flm
113300080115     c                   endif
113400981130     c                   movel     wchca         i10nks
113500981014     c                   movel     dsnumca       i10nkt
113600981014      *
113700981014     c                   movel(P)  dsdn10        kpjbu
113800981014     C                   CALL      'FIDN10R'
113900981014     C                   PARM                    kpjba
114000080115     c                   movel     kpjbu         dsdn10
114100981014      *
114200981014     c                   ENDSR
114300981015      ****************************************************************
114400981015      * F14 = INTERROGAZIONE C.A.
114500981015      ****************************************************************
114600981015     C     SUB_F14       BEGSR
114700981016      *
114800981016     c                   clear                   FIDN00DS
114900981016      *
115000981016     c                   movel     'I'           I00mod
115100981130     c                   movel     wchca         I00fca
115200981016     c                   z-add     dctgfc        I00fgs
115300981016     c                   z-add     dctaac        I00aac
115400981016     c                   z-add     dctfil        I00fil
115500981016     c                   z-add     dctnca        I00nca
115600981016     c                   z-add     dctaas        I00aas
115700981016     c                   z-add     dctlnp        I00lnp
115800981016     c                   z-add     dctnrs        I00nrs
115900981016     c                   z-add     dctnsp        I00nsp
116000981016     c                   z-add     dctlna        I00lna
116100981016     c                   movel     dcttad        I00tad
116200981230     c                   if        dctgfc = W§dctlnpc
116300981016     c                   movel     'P'           I00tpb
116400981016     c                   endif
116500981016     c                   if        dctgfc = dctlna
116600981016     c                   movel     'A'           I00tpb
116700981016     c                   endif
116800981016     c                   if        dctgfc = 046
116900981016     c                   movel     'S'           I00tpb
117000981016     c                   endif
117100981016      *
117200981016     c                   movel     FIDN00DS      kpjbu
117300981016      *
117400981016     c                   call      'FIDN01R'
117500981016     c                   parm                    kpjba
117600981016      *
117700981015     c                   endsr
117800990311      *****************************************************************
117900990311      * CONTROLLO DETTAGLIO COLLI
118000990311      *****************************************************************
118100990311     C     SUB_COLLI     BEGSR
118200990311
118300990311      * Lettura del dettaglio colli per verificare se uguale a quello indicato
118400990311      * in testata
118500990311     c                   clear                   wnco
118600990311     c*
118700990311     c     keynca        setll     fndcd01l
118800990311     c     keynca        reade(n)  fndcd01l                               32
118900990311      *
119000990311     c                   dow       not *in32
119100990311      *
119200990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
119300990412     c* e record non annullato
119400990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
119500990412     c                             and dcdatb = ' '
119600990311     c                   add       1             wnco
119700990311     c                   endif
119800990311q    c*
119900990311     c     keynca        reade(n)  fndcd01l                               32
120000990311     c*
120100990311     c                   enddo
120200990311     c*
120300990311     c                   endsr
120400980608      *****************************************************************
120500980923      * AGGIORNO ARCHIVI
120600980608      *****************************************************************
120700980923     C     SUB_confer    BEGSR
120800981007
120900981015      * note fase ca
121000080115      * (se NON già confermate in subr. "GesD02")
121100080115if  1c                   IF             *in08
121200080115     c                             or   V1Ccch <> WchBM
121300981015     C                   CLEAR                   DSDN10
121400981015     c                   z-add     Wdataca       i10dta
121500981015     C                   movel     'C'           I10FLM
121600080115     c                   eval      I10tpd = 'C'
121700981015     c                   movel(P)  dsnumca       i10nkt
121800981015     c                   movel     Wchca         i10nks
121900981015     c                   movel     'N'           i10trc
122000981015     c                   movel(P)  dsdn10        kpjbu
122100981015     C                   CALL      'FIDN10R'
122200981015     C                   PARM                    kpjba
122300080115e   1c                   ENDIF
122400050404      *
122500050404     c* chiusura C.A.
122600050404     c                   if        *in08 = *off
122700050404     c                   exsr      sub_coch
122800050404     c*
122900050404     c                   else
123000050404     c* riapertura
123100050404     c                   exsr      sub_cori
123200050404     c                   endif
123300981009      *
123400980923     c                   ENDSR
123500981014      *****************************************************************
123600981014      * AGGIORNO ARCHIVI   CHIUSURA C.A.
123700981014      *****************************************************************
123800981014     C     SUB_coch      BEGSR
123900020129      *
124000020129     c                   movel     DCTflo        DDCT01
124100020226      *
124200120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
124300120723     c                   clear                   §DCTspgd
124400120723     c                   movel     DDCT01        DCTflo
124500120723     c                   clear                   DCTpgd
124600981014      *
124700981014      * Imposto campi per trasmissione dati
124800981019     c                   z-add     dateu         DCTdch
124900981130     c                   z-add     wfaca         DCtfca
125000981014     c                   move      v1ccch        DCTcch
125100981222     c                   clear                   DCTft1
125200981222     c                   clear                   DCTft2
125300981014      *
125400981014      * Aggiorno testata C.A.
125500981014     c                   EXCEPT    UPDdct
125600981014      *
125700981014      * Aggiorno l dettaglio C.A.
125800981014     c                   EXSR      UPDDCD
125900050126      *
126000050511      * se sto chiudendo una pratica con causale 17 o 18 aggiorno FNDCL
126100080115      *                                            (o 19)
126200080114     c                   if            V1Ccch = WchBA
126300050511     c                             or  V1Ccch = WchBB
126400080115     c                             or  V1Ccch = WchBM
126500050126     c                   exsr      UPDDCL1
126600050126     c                   endif
126700981014      *
126800981014      * Scrivo fasi C.A.
126900981014     c                   EXSR      WRTDCF
127000051124      * Se programma non richiamato  e
127100131011      * Se chiusa C.A. legata ad Evento con data segnalazione ad A.I.G.
127200131011      *                               o in fase di rimborso A.I.G.
127300131011      *         o C.A. già affidata ad A.I.G. (con importo > §STDLAE)
127400131011      * => emetto window con msg: "C.A. per cui chiedere recupero A.I.G."
127500051018      *                           "Fotocopiare progetto liquidazione "
127600051124     c                   if        i22tri = '  '
127700080114      *
127800051124     c                   if        DCTnev   > *zeros
127900051020     c     K02DET01      chain(n)  FNDET000
128000051020     c                   if        NOT %found(FNDET01L)
128100051020     c                             or  DETatb <> *blanks
128200051020     c                   clear                   DETdtr
128300051020     c                   clear                   DETftr
128400051020     c                   endif
128500051117     c                   endif
128600051020     c                   if             DETdtr   > *zeros
128700051018     c                              or  DETftr   = 'S'
128800051020     c                              or  §DCTaffi = 'A'
128900051018     c                   exfmt     FI22W01
129000051018     c                   endif
129100981014      *
129200051124     c                   endif
129300070404
129400070404      * se filiale gestione non è sede
129500070404    1c                   if        v1cfgs <> 046
129600070404      * se filiale gestione è abilitata alle telefonate GEO
129700070404    2c                   if        wfilgeo = *on
129800070404      * se LNA della spedizione in £6
129900070404     c     dctlna        lookup    l6                                     31
130000170427    3c                   if        *in31 and d50val_a='S'
130100070404      * controllo la data consegna merce e la distinta consegna
130200070404     c                   if        not %open(fnarb01l)
130300070404     c                   open      fnarb01l
130400070404     c                   endif
130500070404     c     karb          chain(n)  fnarb01l
130600070404    4c                   if        %found(fnarb01l) and
130700070404     c                             arbndc = *zeros and arbdcm = *zeros
130800070404     c                   clear                   dslr02
130900070404     c                   eval      dr2aas = dctaas
131000070404     c                   eval      dr2lnp = dctlnp
131100070404     c                   eval      dr2nrs = dctnrs
131200070404     c                   eval      dr2nsp = dctnsp
131300070404     c                   eval      dr2fgs = v1cfgs
131400070404     c                   eval      dr2ric = '1'
131500070404     c                   eval      kpjbu = dslr02
131600070404     c                   call      'FNLR39R'
131700070404     c                   parm                    kpjba
131800070404     c                   eval      dslr02 = kpjbu
131900070404    4c                   endif
132000070404     c                   if        %open(fnarb01l)
132100070404     c                   close     fnarb01l
132200070404     c                   endif
132300070404    3c                   endif
132400070404    2c                   endif
132500070404    1c                   endif
132600051124      *
132700981014     c                   ENDSR
132800981014      *****************************************************************
132900981014      * AGGIORNO ARCHIVI   RIAPERTURA C.A.
133000981014      *****************************************************************
133100981014     C     SUB_cori      BEGSR
133200990311      *
133300990311      * Aggiorno il dettaglio C.A. che ha la data e causale chiusura uguale a quella della testata
133400990311     c     keynca        setll     fndcd01l
133500990311     c     keynca        reade     fndcd01l                               32
133600990311      *
133700990311     c                   dow       not *in32
133800990311      *
133900990311     c* controllo se la causale di chiusura del dettaglio collo è la stessa della testata
134000990311     c*
134100990311     c                   if        dcddch = dctdch  and dcdcch = dctcch
134200990311     c                   clear                   DCddch
134300990311     c                   clear                   DCdcch
134400990311     c                   clear                   DCdftr
134500990311     c                   update    fndcd000
134600990311     c                   endif
134700990311q    c*
134800990311     c     keynca        reade     fndcd01l                               32
134900990311     c*
135000990311     c                   enddo
135100120507      *?Se riapertura di un Porto Franco Import EEX chiuso con CAU 06
135200120507     c                   IF        wPFIEEX
135300120507      *?Cerco il codice del partner da impostare come beneficiario alternativo
135400120507     c                   clear                   FIDN40DS
135500120507     c                   eval      I40dsp = Wdaspe
135600120507     c                   eval      I40fil = dctlnp
135700120507     c                   call      'FIDN40R'
135800120507     c                   parm                    fidn40ds
135900120507      *?Ho trovato il codice del partner imposto la Fase e il beneficiario alternativo
136000120507     c                   IF        O40cod <> *zeros
136100120507     c                   eval      wfaca = wAVVD
136200120507     c                   eval      DCTksc = O40cod
136300120508      *?Devo anche recuperare il numero di pratica
136400120508     c                   IF        DCTprn = *zeros
136500120508     c                   exsr      NUMPRN
136600120508     c                   ENDIF
136700120509     c                   ENDIF
136800120507     c                   ENDIF
136900020131      *
137000981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
137100080123     c                   eval      wSAVcch = DCTcch
137200981014     c                   z-add     *zeros        DCTdch
137300981130     c                   z-add     wfaca         DCtfca
137400080701      * verifico se CA in fase 200 ormai obsoleta da Giugno 2008 imposto la filiale di gestione
137500080701      * in 046 sede per far eseguire la fase 230
137600080701     c                   if        dctfca = 200
137700080701     c                   eval      dctgfc = 46
137800080701     c                   endif
137900080701
138000981014     c                   clear                   DCTcch
138100981222     c                   clear                   DCTft1
138200981222     c                   clear                   DCTft2
138300120723      *
138400120723      * Campi §DCTSPGD (della DS DDCT01) e DCTPGD: NON più usati.
138500120723     c                   clear                   DCTpgd
138600120723     c                   clear                   §DCTspgd
138700120723     c                   movel     DDCT01        DCTflo
138800020131      *
138900050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
139000020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
139100020131      *   causale "15"
139200020131      * => pulisco i flag per l'estrazione
139300050420      *   ADESSO PULISCO SEMPRE ***
139400990430      *
139500050420     c***                if        wfaca = writi or wfaca = wripl
139600050420    1c****                         or $Franch = *on
139700990430     c* pulisco il flag dctpaga nel campo dctflo
139800990430     c                   clear                   §dctpaga
139900990430     c                   movel     ddct01        dctflo
140000050420     c*****              endif
140100050218
140200050218      * Se sono decorsi i gg. da tab. 5A dalla data di chiusura
140300050218      * pulisco il campo dctdt2
140400050218    2c                   if        $over4m = *on
140500050218     c                   Clear                   dctdt2
140600050218    2c                   endif
140700020131      *
140800990430      * Aggiorno testata C.A.
140900990430     c                   EXCEPT    UPDdct
141000020131      *
141100050331      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
141200020131      * o in caso di riapertura C.A. con flag di franchigia = 'F' e
141300020131      *   causale "15"
141400020131      * => pulisco i flag per l'estrazione
141500990430      *
141600050331     c                   if        wfaca = writi  or  wfaca = wripl
141700020131    1c                             or $Franch = *on
141800080123     c                             or wSAVcch = WchBM
141900990430      * Aggiorno testata C.A. importi solo per ritorno al pagamento
142000990430     c                   EXsr      UPDdcl
142100990430     c                   endif
142200050805      *
142300131011      * Ripulisco il flag per richiesta rimborso ad A.I.G. su FNDET00F
142400050805      *   (DETFTR) se quest'ultimo è già impostato a "P" o "N"
142500050805     c                   exsr      UpdDET
142600981014      *
142700981014      * Scrivo fasi C.A.
142800981014     c                   EXSR      WRTDCF
142900010926      *
143000050218      * Se sono decorsi i gg. della tab. 5A dalla data chiusura
143100090417      * aggiorno il file FnRIG00R - rigenerazione bolle x P.O.
143200010926    2c                   if        $over4m = *on
143300010926     c                   EXSR      WRTfirig
143400010926    2c                   endif
143500020128      *
143600050401      * Se è un ritorno al pagamento anche in caso di causale 17 (surroga)
143700050401     c                   if        wfaca = writi  or  wfaca = wripl
143800050401      * scrivo note estremi aseegno rifiutato
143900050401     c                   EXsr      WRTDCS
144000050401     c                   endif
144100981014      *
144200981014     c                   ENDSR
144300990430      *****************************************************************
144400990430      * AGGIORNO FNDCL Testata C.A. Importi
144500990430      *****************************************************************
144600990430     C     UPDDCL        BEGSR
144700990430     c*
144800990430      * pulisco i campi relativi al pagamento ma lascio gli importi
144900020201      * => NON IN CASO DI FRANCHIGIA
145000990430      *
145100990430     c* apro e aggancio  il file liquidazione ca
145200010508      *
145300010508     C                   if        not %OPEN(Fndcl01L)
145400990430     c                   open      fndcl01l
145500010508     c                   endif
145600010508      *
145700990430     c     keynca        chain     fndcl01l                           30
145800010508      * pulisco  i dati relativi il pagamento (ABI/CAB, numero assegno, dta pagamento)
145900010508     c                   clear                   dclfic
146000990430     c                   clear                   dclabi
146100990430     c                   clear                   dclcab
146200990430     c                   clear                   dclasn
146300990430     c                   clear                   dcldea
146400020128      * pulisco le date relative alla stampa ed accettazione progetto di liquidazione
146500080114     c                   IF        $Franch = *on
146600020128     c                   clear                   DCLdds
146700020128     c                   clear                   DCLdpl
146800080114     c                   clear                   DCLipl
146900080114     c                   clear                   DCLvpl
147000080114     c                   ENDIF
147100080123      *
147200080123      * Ripulisco l'importo eventualmente liquidato
147300080123     c                   IF        wSAVcch = WchBM
147400080123     c                   clear                   DCLipl
147500080123     c                   clear                   DCLvpl
147600080123     c                   clear                   DCLvlp
147700080123     c                   ENDIF
147800080123      *
147900990430     c                   clear                   dclftr
148000080123     c  n30              update    fndcl000
148100010508      *
148200010508     C                   if        %OPEN(Fndcl01L)
148300010508     c                   close     fndcl01l
148400010508     c                   endif
148500150702
148600150702      /free
148700150702       //?Cancella rcd 'P' in FNDKA, beneficiario
148800150702         chain (kaac:kfil:knca:'P') FNDKA01L;
148900150702         IF  %found(FNDKA01L);
149000150702           delete FNDKA000;
149100150702         ENDIF;
149200150702      /end-free
149300990430     c*
149400990430     c                   endsr
149500050805
149600050805      **********************************************************************
149700050805      *  AGGIORNAMENTO TESTATA EVENTI
149800050805      **********************************************************************
149900050805     C     UpdDET        BEGSR
150000050805      *
150100131011      * Ripulisco il flag di RIMBORSO A.I.G. su FNDET00F
150200131011      * se  "P" = Ricezione totale del rimborso da A.I.G.
150300050805      *  o  "N" = Evento non fortuito che ha le CA pagate ma < 10000€
150400050805      *
150500050805     c     K02DET01      chain     FNDET000
150600050805      *
150700050805if  1c                   if        %found(FNDET01L)
150800050805     c                             and (DETftr = 'P'
150900050805     c                              or  DETftr = 'N')
151000050805     c                   clear                   DETftr
151100050805     c                   UPDATE    FNDET000
151200050805e   1c                   endif
151300050805      *
151400050805     C                   ENDSR
151500050126      *****************************************************************
151600050126      * AGGIORNO FNDCL data accettazione P.L.
151700050126      *****************************************************************
151800050126     C     UPDDCL1       BEGSR
151900050126      *
152000050126     c* apro e aggancio  il file liquidazione ca
152100050126      *
152200050126     C                   if        not %OPEN(Fndcl01L)
152300050126     c                   open      fndcl01l
152400050126     c                   endif
152500050126      *
152600050126     c     keynca        chain     fndcl01l                           30
152700080115      *
152800080115      * Imposto l'importo liquidato
152900080115     c                   IF        NOT  *in08
153000080115     c                             and  V1Ccch = WchBM
153100080123      *
153200080115     c                   if        *in30
153300080115     c                   clear                   FNDCL000
153400080115     c                   eval      DCLaac = Kaac
153500080115     c                   eval      DCLfil = Kfil
153600080115     c                   eval      DCLnca = Knca
153700080115     c                   endif
153800080123     c                   if        V2Cipl > *zeros
153900080115     c                   eval      DCLipl = V2Cipl
154000080115     c                   eval      DCLvpl = V2Cvpl
154100080115     c                   else
154200080115     c                   clear                   DCLipl
154300080115     c                   clear                   DCLvpl
154400080115     c                   endif
154500080123     c                   eval      DCLvlp = DateU
154600080123      *
154700080123     c                   ELSE
154800080123      *
154900050126     c                   z-add     dateu         dcldpl
155000080123      *
155100080123     c                   ENDIF
155200080123      *
155300050126     c                   clear                   dclftr
155400080115      *
155500080115     c                   select
155600080115     c                   when      NOT *in30
155700080115     c                   UPDATE    FNDCL000
155800080115     c                   when      *in30  and  DCLipl > *zeros
155900080115     c                   WRITE     FNDCL000
156000080115     c                   endsl
156100050126      *
156200050126     C                   if        %OPEN(Fndcl01L)
156300050126     c                   close     fndcl01l
156400050126     c                   endif
156500050126     c*
156600050126     c                   endsr
156700981014      *****************************************************************
156800981014      * AGGIORNO FNDCD Dettaglio C.A.
156900981014      *****************************************************************
157000981014     C     UPDDCD        BEGSR
157100981014     c*
157200981014     c     keynca        setll     fndcd01l
157300981014     c     keynca        reade     fndcd01l                               31
157400981014     c*
157500981014      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
157600981014     c                   dow       *in31 = *off
157700981014     c                   eval      DCddch = dctdch
157800981014     c                   eval      DCdcch = dctcch
157900981222     c                   eval      DCdftr = dctft1
158000981014     c                   update    fndcd000
158100981014     c*
158200981014     c     keynca        reade     fndcd01l                               31
158300981014     c*
158400981014     c                   enddo
158500981014     c*
158600981014     c                   endsr
158700980923      *****************************************************************
158800980923      * AGGIORNO FNDCF FASI C.A.
158900980923      *****************************************************************
159000980923     C     WRTDCF        BEGSR
159100980923      *
159200990226     c                   clear                   fndcf000
159300020201     c                   clear                   DDCF01
159400980923     C                   z-add     DCTaaC        DCFaac
159500980923     C                   z-add     DCTfil        DCFfil
159600980923     C                   z-add     DCTnca        DCFnca
159700981014     c                   z-add     dctfca        DCFfca
159800980923     c                   z-add     dateu         DCFdfc
159900980923     c                   z-add     wora          DCFhfc
160000980923     c                   z-add     V1Cfgs        DCFfev
160100981026     c                   movel     dctptr        DCFptr
160200980923     c                   movel     knmus         DCFpru
160300020201     c                   movel     §DCTpfra      §DCFpfra
160400020201     c                   movel     DDCF01        DCFlet
160500980923      *
160600980923     c                   WRITE     FNDCF000
160700980923      *
160800980923     c                   ENDSR
160900981105      *****************************************************************
161000981105      * AGGIORNO ARCHIVI   RIAPERTURA C.A. PER RIFIUTO TRANSAZIONE
161100981105      *****************************************************************
161200981105     C     SUB_RIFIU     BEGSR
161300990312      * disalloco il file fndct
161400990312     c                   unlock    fndct01l
161500981105      * Richiamo il pgm rifiuto transazione
161600981105      *
161700981105     c                   clear                   FIDN00DS
161800981105      *
161900981105     c                   movel     'G'           I00mod
162000981105     c                   movel     wrifi         I00fca
162100981105     c                   z-add     dctgfc        I00fgs
162200981105     c                   z-add     dctaac        I00aac
162300981105     c                   z-add     dctfil        I00fil
162400981105     c                   z-add     dctnca        I00nca
162500981105     c                   z-add     dctaas        I00aas
162600981105     c                   z-add     dctlnp        I00lnp
162700981105     c                   z-add     dctnrs        I00nrs
162800981105     c                   z-add     dctnsp        I00nsp
162900981105     c                   z-add     dctlna        I00lna
163000981105     c                   movel     dcttad        I00tad
163100981230     c                   if        dctgfc = W§dctlnpc
163200981105     c                   movel     'P'           I00tpb
163300981105     c                   endif
163400981105     c                   if        dctgfc = dctlna
163500981105     c                   movel     'A'           I00tpb
163600981105     c                   endif
163700981105     c                   if        dctgfc = 046
163800981105     c                   movel     'S'           I00tpb
163900981105     c                   endif
164000981105      *
164100981105     c                   movel     FIDN00DS      kpjbu
164200981105      *
164300981105     c                   call      'FIDN23R'
164400981105     c                   parm                    kpjba
164500990312     c                   movel     kpjbu         fidn00ds
164600990312     c*
164700990312      *
164800990312     C     Keynca        chain     FNDCT000                           31
164900990312      *
165000990312      * se non viene dato KL/KC  in FIDN23 aggiorno altrimenti riemetto la videata
165100990312     c                   if        o00f12 = ' ' and o00f03 = ' '
165200990312      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
165300990312     c                   z-add     *zeros        DCTdch
165400990312     c                   clear                   DCTcch
165500990312     c                   clear                   DCTft1
165600990312     c                   clear                   DCTft2
165700990312      *
165800990312      * Aggiorno testata C.A.
165900990312     c                   EXCEPT    UPDdct
166000990312      * Aggiorno l dettaglio C.A.
166100990312     c                   EXSR      UPDDCD
166200990312      *
166300990312     c                   endif
166400981105      *
166500981105     c                   ENDSR
166600050331      *****************************************************************
166700050331      *  SCRITTURA NOTE AGGIUTIVE
166800050331      *****************************************************************
166900050331     C     WRTDCS        BEGSR
167000050331      *
167100050331      * cerco l'ultimo progressivo note della fase in corso
167200050331      * - campi strandard
167300050331     c                   eval      Ktpd = 'C'
167400050331     c                   eval      aac = kaac
167500050331     c                   eval      fil = kfil
167600050331     c                   eval      nca = knca
167700050331     c                   eval      Knkt = dsnumca
167800050331     c                   eval      Kstd = ' '
167900050331     c                   eval      Kdim = dateu
168000050401     c                   eval      Khim = wora
168100050331      * - dati relativi alla CA in manutenzione
168200050331     c                   movel(p)  900           Knks
168300050331     c                   eval      Ktrc = 'N'
168400050331      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
168500050331     c     Kdcs          setgt     FNDCS01L
168600050331     c     Kdcs          readpe    FNDCS01L
168700050331      *
168800050331     c                   clear                   DCSatb
168900050331     c                   eval      DCStpd = Ktpd
169000050331     c                   eval      DCSnkt = Knkt
169100050331     c                   eval      DCSstd = Kstd
169200050331     c                   eval      DCSdim = Kdim
169300050331     c                   eval      DCShim = Khim
169400050331     c                   eval      DCSnks = Knks
169500050331     c                   eval      DCStrc = Ktrc
169600050331     c                   IF        %eof(FNDCS01L)
169700050331     c                   eval      DCSpno = 1
169800050331     c                   else
169900050331     c                   eval      DCSpno = DCSpno + 1
170000050331     c                   ENDIF
170100050401     c                   eval      DCSnot ='Il Cliente ha rifiutato l''assegno '
170200050331     c                   eval      DCSpru = KNMUS
170300050331     c                   eval      DCSpos = v1cfgs
170400050331     c                   clear                   DCSstn
170500050331     c                   clear                   DCSft1
170600050331     c                   clear                   DCSdt1
170700050331      *
170800050331     c                   write     FNDCS000
170900050401      * 2° riga
171000050401     c                   eval      DCSpno = DCSpno + 1
171100050401     c                   clear                   dcsnot
171200050401     c
171300050401     c                   eval      DCSnot = 'di traenza n°' + %trim(v1casn) +
171400050401     c                                      ' emesso il '
171500050401
171600050401     c                   write     FNDCS000
171700050401      * 3° riga
171800050401     c                   eval      DCSpno = DCSpno + 1
171900050401     c                   clear                   dcsnot
172000050401     c
172100050401     c                   eval      DCSnot = %editw(v1cdea:'  /  /    ') + ' di '
172200050401     c                             + %trim(%editc(v1cipl:'3')) + ' ' + v1cvpl
172300050331      *
172400050401
172500050401     c                   write     FNDCS000
172600050331     C                   ENDSR
172700120508      *****************************************************************
172800120508      *   REPERISCO NUMERO PRATICA ASSICURATIVA
172900120508      *****************************************************************
173000120508     C     NUMPRN        BEGSR
173100120508
173200120508      * verifico se CA legata ad evento
173300120508      * se legata verifico se nel file delle denucnce c'è il suo numero pratica
173400120508     c                   If        dctnev > 0
173500120508     c     K02DET01      chain     fndet01l
173600120508      * non trovo sul file denuncia vado via
173700120508     c                   If        not %found(fndet01l)
173800120508     c                   leavesr
173900120508     c                   endif
174000120508      * c'è il numero pratica imposto i campi e poi vado via
174100120508     c                   If        detprn > 0
174200120508     c                   unlock    fndet01l
174300120508     c                   eval      dctdit  =   DETdit
174400120508     c                   eval      dctpra  =   DETpra
174500120508     c                   eval      dctprn  =   DETprn
174600120508     c                   leavesr
174700120508     c                   endif
174800120508      * non c'è il numero pratica lo recupero
174900120508     c                   Clear                   FIDN20DS
175000120508     c                   Movel     'N'           I20mod
175100120508     c                   Z-add     DETaae        I20pra
175200120508     c                   Movel     DETdit        I20dit
175300120508     c                   movel(P)  FIDN20DS      KPJBU
175400120508     c                   CALL      'FIDN20R'
175500120508     c                   PARM                    KPJBA
175600120508     c                   movel     KPJBU         FIDN20DS
175700120508      * torna errore vado via
175800120508     c                   IF        O20err = 'E'
175900120508     c                   leavesr
176000120508     c                   ENDIF
176100120508      * c'è il numero pratica imposto i campi aggiorno il file denuncia
176200120508     c                   movel     I20dit        DETdit
176300120508     c                   z-add     I20pra        DETpra
176400120508     c                   z-add     O20prn        DETprn
176500120508     c                   update    fndet000
176600120508      * imposto i campi per la testata CA e vado via
176700120508     c                   eval      dctdit  =   DETdit
176800120508     c                   eval      dctpra  =   DETpra
176900120508     c                   eval      dctprn  =   DETprn
177000120508     c                   leavesr
177100120508     c                   endif
177200120508
177300120508      * La CA non è legata ad evento
177400120508      * recupero un numero pratica
177500120508     C                   clear                   FIDN20DS
177600120508     C                   movel     'N'           I20mod
177700120508     c                   z-add     DCTaas        I20pra
177800120508     c                   movel     'BAR'         I20dit
177900120508      * se la spedizione ha valore da assicurare esposto in bolla o
178000120508      *   assicurazione per conto: verifico se il P.O. di arrivo ha
178100120508      *   FEDEX come network
178200120508     c                   if        TASias > 0 and TASfcm <> 'F'
178300120508     c     DCTlna        Chain     AZORG
178400120508if  1c                   If            %found(AZORG01L)
178500120508     c                             and ORGfva = *blanks
178600120508     c                   Movel     ORGde3        OG143
178700120508if  2c                   If        §OGntw = 'FED'
178800120508     c                   Movel     §OGntw        I20dit
178900120508e   2c                   Endif
179000120508e   1c                   Endif
179100120508     c                   endif
179200120508     C                   movel(P)  FIDN20DS      KPJBU
179300120508     c                   CALL      'FIDN20R'
179400120508     c                   PARM                    KPJBA
179500120508     C                   movel     KPJBU         FIDN20DS
179600120508      * torna errore vado via
179700120508     c                   IF        O20err = 'E'
179800120508     c                   eval      *in28 = *ON
179900120508     c                   leavesr
180000120508     c                   ENDIF
180100120508      * imposto i campi per la testata CA
180200120508     c                   movel     I20dit        DCTdit
180300120508     c                   z-add     I20pra        DCTpra
180400120508     c                   z-add     O20prn        DCTprn
180500120508      *
180600120508     c                   ENDSR
180700980910      *****************************************************************
180800980527      * ROUTINE INIZIALE
180900980527      *****************************************************************
181000980527     C     *INZSR        BEGSR
181100980615      *
181200980521     C     *ENTRY        PLIST
181300980521     C                   PARM                    KPJBA
181400981014     c                   movel     kpjbu         fidn22ds
181500981014     c* se numero CA diversa da zeros  vuol dire che il programma è stato richiamato
181600981015     c                   if        i22tri = 'R'
181700981014     c                   eval      v1cfil = i22fil
181800981014     c                   eval      v1caac = i22aac
181900981014     c                   eval      v1cnca = i22nca
182000981021     c                   eval      v1ccch = i22cch
182100981202     c                   eval      v1cfgs = i22fgs
182200981021     c* decodifico causale chiusura se c'è
182300981021     c                   if        v1ccch <> '  '
182400981021     c                   clear                   dsbs02
182500981021     C                   MOVEL     'C'           t02mod
182600981021     C                   MOVEL     knsif         t02sif
182700981021     C                   MOVEL     'CCH'         t02cod
182800981021     C                   MOVEL(P)  v1ccch        t02ke1
182900981021      *
183000981021     C                   CALL      'TIBS02R'
183100981021     C                   PARM                    KPJBA
183200981021     C                   PARM                    dsbs02
183300981021      *
183400981021    4C                   if        t02err = *BLANKS
183500981021     C                   MOVEL     T02UNI        v1dcch
183600981021     c                   endif
183700990122     c*
183800990122     c                   else
183900010508      *
184000010508     C                   if        not %OPEN(Fidn22d)
184100990122     c                   open      fidn22d
184200010508     c                   endif
184300981021      *
184400981021     c                   endif
184500981021      *
184600981021     c* proteggo i campi del cambio p.o gestione e non abilito il CMD3
184700981021     c                   seton                                        060710
184800990122     c*
184900990122     c                   else
185000010508     C                   if        not %OPEN(Fidn22d)
185100010508     c                   open      fidn22d
185200010508     c                   endif
185300981014     c                   endif
185400980907      *
185500050929      * Reperisco dati job
185600050929     c                   exsr      DatiJob
185700050929     c                   eval      TBLkut = 1
185800020128      *
185900020128      * Reperisco la moneta corrente
186000020128     C                   clear                   DSBS02
186100020128     C                   movel     'L'           T02tla
186200020128     C                   movel     'C'           T02mod
186300020128     C                   movel     KNSIF         T02sif
186400020128     C                   movel     'GED'         T02cod
186500020128     C                   movel     'DANNI'       T02ke1
186600020128     C                   call      'TIBS02R'
186700020128     C                   parm                    KPJBA
186800020128     C                   parm                    DSBS02
186900020128     C                   If        T02err = *blanks
187000020128     C                   movel     T02UNI        DGEDDN
187100020128     C                   else
187200020128     C                   clear                   DGEDDN
187300020128     C                   Endif
187400020128      *
187500020128      * Reperisco tab.STD
187600020128     C                   clear                   DSBS02
187700020129     C                   movel     'L'           T02tla
187800020128     C                   movel     'C'           T02mod
187900020128     C                   movel     KNSIF         T02sif
188000020128     C                   movel     'STD'         T02cod
188100020128     C                   movel     '1'           T02ke1
188200020128     C                   movel     §gedDBA       T02ke2
188300020129     C                   call      'TIBS02R'
188400020129     C                   parm                    KPJBA
188500020129     C                   parm                    DSBS02
188600020128     C                   If        T02err = *blanks
188700020128     C                   movel     T02uni        DSTD
188800020128     C                   else
188900020128     C                   clear                   DSTD
189000020128     C                   Endif
189100980907
189200000228      * IMPOSTO IL P.O. IN GESTIONE se = a zeros
189300000228     c                   if        v1cfgs = *zeros
189400050929    2C                   IF           DUTlpo = '2'
189500050929     C                             or DUTlpo = *blanks
189600050929     C                   movel     DUTpou        V1CFGS
189700980521     C                   eval      *in06 = *on
189800980521   X2C                   ELSE
189900980521     C                   movel     SIMFEL        V1CFGS
190000980521    2C                   ENDIF
190100070404     c                   endif
190200981001      *
190300981001      * Se sede non visualizzo  "P.O. IN GESTIONE"
190400981001     C                   IF        V1Cfgs = *zeros
190500981001     C                   eval      *in06 = *on
190600981001     C                   eval      *in07 = *on
190700981001     C                   z-add     46            V1CFGS
190800170517     c                   eval      L6(1)=046
190900070404     c**!!!              ELSE
191000070404     c                   endif
191100070404      * Se non è sede carico £6 è altri dati
191200070404     c                   if        v1cfgs <> 46
191300070404     c                   clear                   og148
191400981012     C     V1CFGS        CHAIN     AZORG                              31
191500980521     C  N31              MOVEL     orgDES        V1DFGS
191600070404     c  n31              eval      og148 = orgde8
191700070404      * controllo se Filiale gestione è abilitata a telefonate GEO
191800070404     c                   eval      wfilgeo = (§oggeot = 'S')
191900170427
192000170427     c* richiamo controllo filiale gestine per impostare anche i flag abilitaz. part/arr
192100170427     c                   exsr      Ctrfgs
192200170427     c
192300170427     C***                exsr      CARL6
192400070404    2C**!!!              ENDIF
192500000228     c                   endif
192600981007      *
192700981007      * Imposto il TITOLO
192800981014      * chiusura
192900981014     c                   if        i22mod = 'C'
193000981014     c                   movel     TIT_chca      VTCtit
193100981130     c* valorizzo la fase di chiusura
193200981130     c                   eval      wfaca = wchca
193300981014     c                   endif
193400981014      * riapertura
193500981014     c                   if        i22mod = 'R'
193600981014     c                   movel     TIT_raca      vtctit
193700981130     c* pulisco la fase
193800981130     c                   clear                   wfaca
193900981014     c                   seton                                        08
194000981014     c                   endif
194100980721      * reperisco data e ora
194200980721     C                   TIME                    W0140
194300980521      * UDATE IN GGMMAAAA
194400980521     C                   MOVE      W0140         WDTGIO
194500980923     C                   MOVEL     W0140         WORA
194600980521      * UDATE IN AAAAMMGG
194700980521     C                   Z-ADD     WDTGIO        G02DAT
194800980521     C                   MOVEL     *BLANK        G02ERR
194900980521     C                   CALL      'XSRDA8'
195000980521     C                   PARM                    WLBDAT
195100980521     C                   MOVEL     G02INV        DATEU
195200980923
195300000117      *    Imposto anno C.A.  se non sono stato richiamato
195400000117     c                   if        i22tri <> 'R'
195500981013     c                   move      wdtgio        V1Caac
195600000117     c                   endif
195700050218
195800050218      * Aggancio tabella 5A rcd 2
195900050218     c                   Clear                   ds5a2
196000050218     c                   Eval      tblcod = '5A'
196100050218     c                   Eval      tblkey = '2       '
196200050218     c     ktab          Chain     Tabel00f
196300050218     c                   If        %Found(Tabel00f)
196400050218     c                   Eval      ds5a2 = TblUni
196500050218     c                   EndIf
196600980721
196700981014     C     Keynca        klist
196800981013     C                   KFLD                    kaac
196900981013     C                   KFLD                    kfil
197000981013     C                   KFLD                    knca
197100050805      * File Eventi
197200050805     c     K02DET01      klist
197300050805     c                   kfld                    DCTaae
197400050805     c                   kfld                    DCTnev
197500981016      * File giacenza
197600981016     c     kgiac         klist
197700040722     c                   kfld                    giaaas
197800040722     c                   kfld                    gialnp
197900040722     c                   kfld                    gianrs
198000040722     c                   kfld                    giansp
198100981016     c                   kfld                    Wfrg
198200981016      * File Bolle
198300981016     c     karb          klist
198400981016     c                   kfld                    dctaas
198500981016     c                   kfld                    dctlnp
198600981016     c                   kfld                    dctnrs
198700981016     c                   kfld                    dctnsp
198800010926      * file TITAS30C
198900010926     c     K04TAS30C     klist
199000010926     c                   kfld                    DCTaas
199100010926     c                   kfld                    DCTlnp
199200010926     c                   kfld                    DCTnrs
199300010926     c                   kfld                    DCTnsp
199400050218
199500050218     c     ktab          klist
199600050929     c                   kfld                    TBLkut
199700050218     c                   kfld                    tblcod
199800050218     c                   kfld                    tblkey
199900050331      * file FNDCS00F
200000050331     C     KDCS          KLIST
200100050331     C                   KFLD                    KTPD
200200050331     C                   KFLD                    KNKT
200300050331     C                   KFLD                    KSTD
200400050331     C                   KFLD                    KDIM
200500050331     C                   KFLD                    KHIM
200600050331     C                   KFLD                    KNKS
200700050331     C                   KFLD                    KTRC
200800980915
200900980521     C                   ENDSR
201000050929      *
201100050929      *---------------------------------------------------------------*
201200050929      * Reperimento Dati del job (Utente/Operativi)                   *
201300050929      *---------------------------------------------------------------*
201400050929     c     DatiJob       BEGSR
201500050929      *
201600050929     c     *dtaara       define    §azute        azuteds
201700050929     c     *dtaara       define    §datiute      ddatiute
201800050929      *
201900050929     c                   in(E)     *dtaara
202000050929     c                   IF        %ERROR or RSUT = *blanks
202100050929     c                   clear                   Tibs34Ds
202200050929     c                   call      'TIBS34R'
202300050929     c                   parm                    Tibs34Ds
202400050929     c                   in        *dtaara
202500050929     c                   ENDIF
202600050929      *
202700050929     c                   ENDSR
202800010926
202900010926      *****************************************************************
203000011008      * AGGANCIO FILE TITAS30C (PER CONTROLLO DATA BOLLA, MA NON SOLO)
203100010926      *****************************************************************
203200011008     c     ChaTITAS      BEGSR
203300010926      *
203400010926     c                   if        not %OPEN(TITAS30C)
203500010926     c                   open      TITAS30C
203600010926     c                   endif
203700010926      *
203800010926     c     K04TAS30C     chain     TITAS30C
203900011008     c                   if        not %found
204000011008     c                   clear                   TASaas
204100011008     c                   clear                   TASlnp
204200011008     c                   clear                   TASnrs
204300011008     c                   clear                   TASnsp
204400011008     c                   clear                   TASlna
204500010926     c                   endif
204600010926      *
204700010926     c                   ENDSR
204800010926
204900010926      *****************************************************************
205000090417      * AGGIORNAMENTO FILE FnRIG00f
205100010926      *****************************************************************
205200010926     c     WRTfirig      BEGSR
205300010926      *
205400090417     c                   if        not %OPEN(FnRIG00F)
205500090417     c                   open      FnRIG00F
205600010926     c                   endif
205700010926      *
205800090417     c                   clear                   FnRIG000
205900010926     c                   eval      RIGaas = TASaas
206000010926     c                   eval      RIGlnp = TASlnp
206100010926     c                   eval      RIGnrs = TASnrs
206200010926     c                   eval      RIGnsp = TASnsp
206300010926     c                   eval      RIGlna = TASlna
206400090417     c                   eval      RIGcau = 'D'
206500010926     c                   eval      RIGtra = 'E'
206600090417     c                   eval      RIGfil = V1Cfgs
206700010926     c                   eval      RIGpru = KNMUS
206800090417     c                   eval      RIGdata = Dateu
206900090417     c                   eval      RIGora = wora
207000010926      *
207100090417     c                   write     FnRIG000
207200010926      *
207300010926      * ...e rigenero la bolla legata alla C.A. riaperta
207400010926     c                   clear                   KPJBU
207500010926     c                   eval      kcoaz = 'BS21'
207600010926     c                   call      'BCH10'
207700010926     c                   parm                    kpjba
207800020128      *
207900010926     c                   ENDSR
208000080114      *
208100080114      *****************************************************************
208200080114      * Gestione videata D02
208300080114      *****************************************************************
208400080114     c     GesD02        BEGSR
208500080114      *
208600080114      * Inizializzazione videata
208700080114     c                   if        $InzD2 = *on
208800080114     c                   exsr      InzD02
208900080114     c                   eval      $InzD2 = *off
209000080114     c                   endif
209100080114      * Emissione videata
209200080114     c                   if        NOT *in90
209300080114     c                   write     FI22T01
209400080114     c                   endif
209500080114     c                   exfmt     FI22D02
209600080114     c                   setoff                                       28  90
209700080114     c                   clear                   V1Cmsg
209800080114      *
209900080114     c                   SELECT
210000080114      * F3=Fine
210100080114     c                   WHEN      *inKC
210200080114     c                   reset                   $Video
210300080114     c                   eval      O22f03 = 'S'
210400080114      * F12=Ritorno
210500080114     c                   WHEN      *inKL
210600080114     c                   reset                   $Video
210700080114      * F10=Note C.A.
210800080114     c                   WHEN      *inKJ
210900080114     c                   exsr      Sub_F10
211000080114      * F14=Interrogazione C.A.
211100080114     c                   WHEN      *inKN
211200080114     c                   exsr      Sub_F14
211300080114      * Invio / F6
211400080114     c                   OTHER
211500080114     c                   exsr      CtrD02
211600080115     c                   if        NOT *in90  and  *inKF
211700080115      * - Conferma delle eventuali note precedentemente inserite
211800080115     c                   clear                   dsDN10
211900080115     c                   eval      I10dta = WdataCA
212000080115     c                   eval      I10flm = 'C'
212100080115     c                   eval      I10tpd = 'C'
212200080115     c                   movel(p)  dsNumCA       I10nkt
212300080115     c                   movel(p)  WchCA         I10nks
212400080115     c                   eval      I10trc = 'N'
212500080115     c                   movel(p)  dsDN10        kpjbu
212600080115     c                   call      'FIDN10R'
212700080115     c                   parm                    kpjba
212800080115     c                   movel     kpjbu         dsDN10
212900080115      * - Note fase ca
213000080115     c                   exsr      Sub_F10
213100080115      * - Uscita
213200080115     c                   clear                   $Video
213300080115     c                   endif
213400080115     c                   ENDSL
213500080114      *
213600080114     c                   ENDSR
213700080114      *
213800080114      *****************************************************************
213900080114      * Inizializzazione videata D02
214000080114      *****************************************************************
214100080114     c     InzD02        BEGSR
214200080114      *
214300080114     c                   movea     *in           Sav_In
214400080114     c                   clear                   FI22D02
214500080114     c                   movea     Sav_In        *in
214600080114      *
214700080114      * - Riporto i dati da D01
214800080114     c                   eval      V2Cfgs = V1Cfgs
214900080114     c                   eval      V2Dfgs = V1Dfgs
215000080114     c                   eval      V2Cfil = V1Cfil
215100080114     c                   eval      V2Cnca = V1Cnca
215200080114     c                   eval      V2Caac = V1Caac
215300080114     c                   eval      V2Ccch = V1Ccch
215400080114     c                   eval      V2Dcch = V1Dcch
215500080114      *
215600080114      * - Importo massimo risarcibile
215700080114     c                   eval      V2Cipv = DCTipv
215800080114     c                   eval      V2Cvpv = DCTvpv
215900080123      * - Tipo Calcolo
216000080123     c                   select
216100080123     c                   when      DCTfpv = 'M'
216200080123     c                   eval      V2Dfpv = 'Inserito manualmente'
216300080123     c                   when      DCTfpv = 'C'
216400080123     c                   eval      V2Dfpv = 'Calcolato'
216500080123     c                   endsl
216600080114      *
216700080114      * Reperimento "importo richiesto" ed "importo liquidato"
216800080114if  1c                   if        NOT %open(FNDCL01L)
216900080114     c                   open      FNDCL01L
217000080114e   1c                   endif
217100080114     c     KEYnca        chain(n)  FNDCL000
217200080114if  1c                   IF        %found(FNDCL01L)
217300080114      * - Importo richiesto
217400080114     c                   eval      V2Cipr = DCLipr
217500080114     c                   eval      V2Cvpr = DCLvpr
217600080114      * - Importo liquidato
217700080114     c                   eval      V2Cipl = DClipl
217800080115     c                   eval      V2Cvpl = DCLvpl
217900080114e   1c                   ELSE
218000080114     c                   clear                   V2Cipr
218100080114     c                   clear                   V2Cvpr
218200080114     c                   clear                   V2Cipl
218300080114e   1c                   ENDIF
218400080115     c                   if        V2Cvpl = *blanks
218500080115     c                   eval      V2Cvpl = §gedDBA
218600080115     c                   endif
218700080114if  1c                   if        %open(FNDCL01L)
218800080114     c                   close     FNDCL01L
218900080114e   1c                   endif
219000080114      *
219100080114     c                   ENDSR
219200080114      *
219300080114      *****************************************************************
219400080114      * Controllo dati videata D02
219500080114      *****************************************************************
219600080114     c     CtrD02        BEGSR
219700080114      *
219800080114     c                   movea     *zeros        *in(40)
219900080115      *
220000080115     c                   if        V2Cipl < *zeros
220100080115     c                   seton                                        284390
220200080115     c                   eval      V1Cmsg = Msg(22)
220300080115     c                   leavesr
220400080115     c                   endif
220500080114      *
220600080114     c                   ENDSR
220700080114
220800980521      *---------------------------------------------------------------------------------------------
220900981007     oFNDCT000  E            UPDDCT
221000981013     O                       DCTdch
221100981013     O                       DCTcch
221200981013     O                       DCTfca
221300080701     O                       DCTgfc
221400020226     O                       DCTpgd
221500120507     O                       DCTksc
221600120508     o                       DCTDIT
221700120508     o                       DCTPRA
221800120508     o                       DCTPRN
221900990430     O                       DCTflo
222000981222     O                       DCTft1
222100981222     O                       DCTft2
222200050408     O                       DCTdt2
222300980923      *---------------------------------------------------------------------------------------------
222400980521** MSG  Lungh. 78                                                            *
222500981013Il numero C.A. immesso non esiste
222600981014Il valore immesso nel campo non è valido                                       2
222700981014Causale di chiusura errata                                                     3
222800170427C.A. in gestione ad altra filiale                                              4
222900981014C.A. già chiusa                                                                5
223000981014C.A. non chiusa impossibile riaprirla                                          6
223100981021Impossibile riaprire C.A. : pratica giacenza aperta                            7
223200010926La C.A. non si può più riaprire perchè la bolla è antecedente il 01/09/1999    8
223300981021Impossibile riaprire C.A. : spedizione con blocco                              9
223400990311Non si può usare questa causale per chiusure non automatiche                  10
223500990311C.A. in fase di contabilizzazione impossibile riaprirla                       11
223600990311C.A. in fase di pagamento per Pratica Assicurativa impossibile riaprirla      12
223700990311La C.A. non si può più riaprire per mancanza dettaglio colli                  13
223800050218La C.A. non si può più riaprire perchè decorsi xxx gg. da data chiusura       14
223900050126Non si può usare questa causale se non stampato progetto liquidazione         15
224000050126Causale di chiusura che può usare solo la sede                                16
224100050331Utente non autorizzato ad aprire questa C.A.                                  17
224200050512Causale Chiusura utilizzabile per spedizioni di data &PRE/SUC      17/04/2005 18
224300131011La C.A. non si può riaprire. Avviate richieste di recupero ad A.I.G.          19
224400131011Causale di Pagamento utilizzabile solo per C.A. NON affidata ad A.I.G.        20
224500050929Causale di chiusura utilizzabile solo da utenti di                            21
224600080115L'importo liquidato NON può essere negativo                                   22
