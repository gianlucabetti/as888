000100140508     /*PRM  dbgview(*source)
000200140508     /*PRM  commit(*NONE)
000300140508     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
000400140508     /*CMD         ovrscope(*calllvl)
000500140508     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
000600140508     /*CMD         ovrscope(*calllvl)
000700140508     /*END  dltovr file(*all) lvl(*)
000800140508     /*END
000900140508
001000000316     H DECEDIT('0,') DATEDIT(*DMY/)
001100121016     h dftactgrp(*no)
001200000418      *----------------------------------------------------
001300000316      * Rigenerazione bolle per P.O. da TITAS
001400000418      *----------------------------------------------------
001500000320      *    RICHIESTA RIGENERAZIONE SPEDIZIONE
001600000316      *----------------------------------------------------
001700020903      * A T T E N Z I O N E:
001800020903      *---------------------
001900090423      * VALORI DI RIGCAU
002000020903      * D = DANNI
002100020903      * C = C/A OMESSI
002200020903      * F = BOLLE FIRMATE
002300040510      * S = BOLLE FIRMATE DA SCANNER
002400000418
002500000418      ******************************************************
002600000418      * INDICATORI RISERVATI
002700000418      ******************************************************
002800000418      * 05 - sproteggo il P.O. gestione
002900000418      * 06 - non abilito il tasto funzionale F18
003000000418      * 07 - non emetto il P.O. gestione
003100000418      * 08 - Proteggo numero spedizione e lna se ricevuti dal chiamante
003200020304      * 09 - Richiesta Bolla Firmata
003300000418      * 28 - errore generico DSPF
003400000418      * 30 - comodo
003500000418      * 40 - errore P.O. gestione
003600000418      * 41 - errore spedizione
003700000418      * 42 - errore linea arrivo
003800000418      * 43 - errore causale richiesta
003900020304      * 44 - errore p.o. che effettua la richiesta
004000020903      * 45 - errore Flag addebito M/D
004100000418      * 90 - riemissione formato
004200000418      ******************************************************
004300000418
004400000320     fTIBS20D   CF   E             WORKSTN
004500090417     FFnRIG00F  O  A E             DISK
004600030321     FFNBLP01L  IF   E           K DISK    usropn
004700000317     FFNARB01L  IF   E           K DISK    usropn
004800000317     FTITAS30C  IF   E           K DISK    usropn
004900030321     FTNCSB03L  uF   E           K DISK    usropn
005000000418     FFNLBL01L  IF   E           K DISK    usropn  rename(fnlbl000:fnlbl001)
005100000418     FFNLBL02L  IF   E           K DISK    usropn  rename(fnlbl000:fnlbl002)
005200020201     Ftabel00f  IF   E           K DISK    usropn
005300000320     FAZORG01L  IF   E           K DISK
005400020903     fTitaa30c  if   e           k Disk    usropn
005500090417     fFnrig02l  if   e           k Disk            rename(fnrig000:Fnrig02)
005600121019     fficso00r  o    e           k disk            usropn
005700000316      *----------------------------------------------------
005800000317      *   S C H I E R E
005900121018     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
006000121018     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
006100121018     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
006200040510     D MSG             S             78    DIM(35) CTDATA PERRCD(1)
006300000418     D L1              S              3  0 DIM(30)
006400000418     D L6              S              3  0 DIM(30)
006500000317      *
006600000317      *   D S   I N T E R N E / E S T E R N E
006700000317      *
006800020204     D og143         E DS
006900020204     D TIBS20DS      E DS
007000000320     D FNLV50DS      E DS
007100140508     D*// DSCMZ         E DS
007200121022     D Fidna4ds      E DS
007300140508     D Fidna6ds      E DS
007400000417     D KPJBA         E DS
007500121018     D  LIBSYS                92    101
007600121018     D  LEGFIL               483    492
007700121018     D  MBRFIL               493    502
007800000317     D TRUL06DS      E DS
007900000418     D  LIN                    1     90  0    DIM(30)
008000020703
008100020703     d ds3a          e ds
008200140508     d*// Tibs55Ds      e ds
008300020903     d tibs69ds      e ds
008400020903     d ds_cnaco      e ds                  extname(cnaco00f)
008500020903     d ds_cnind      e ds                  extname(cnind00f)
008600020903     d ds_cnclp      e ds                  extname(cnclp00f)
008700020903     d ds_fncls      e ds                  extname(fncls00f)
008800040510     d dtasflo       e ds
008900040510     d og146         e ds
009000040511
009100040511     d dtaimm          ds                  inz
009200040511     d  anno                   1      4
009300040511     d  mese                   5      6
009400040511     d  giorno                 7      8
009500020903
009600121019     d §membro         ds
009700121018     d  §emme                  1      1    inz('M')
009800121018     d  §fle                   2      4s 0
009900121018     d  §sed                   5      7s 0 inz(046)
010000121018     d  §mbr                   2      7
010100121022     d
010200000317      *
010300000317     D                SDS
010400000317     D  VTCPGM                 1     10
010500000320      *
010600000320      *   C A M P I
010700000320      *
010800000320     D W0140           S             14  0 inz
010900140508     D*// W236            S            236
011000140508     D*// cmzlcl          S              1
011100000418     D wlegame         S              1
011200000418     D Wrec            S              7  0
011300000320     D Wdtgio          S              8  0
011400000320     D dateu           S              8  0
011500000320     D Wora            S              6
011600121025     D skpjbu          S                   LIKE(kpjbu)
011700000321     D wfgs            S                   LIKE(V1Cfgs)
011800000418     D Wi20vid         S                   LIKE(I20vid)
011900020213     D Wdata           S               D   DATFMT(*eur)
012000020507     d wdataold        S               d   datfmt(*eur)
012100020507     d wolddata        s              8  0
012200020507     d wtasdta         s              8  0
012300020213     D OUTQA           S             10
012400020213     D OUTQP           S             10
012500020213     D COM03           S              3
012600020304     d flagpo          s              1
012700020903     d Wtrc            s              1
012800020903     d Wtb1            s              1
012900020903     d Wccm            s              4  0
013000020903     d Wccd            s              4  0
013100021014      *parametri per reperimento coda
013200021014     d finep           s              1
013300021014     d codpo           s              3s 0
013400021014     d tpcoda          s              1
013500021014     d libcoda         s             10
013600021014     d esito           s              1
013700021014     D OUTQ            S             10
013800040511
013900040511     d wforza          s              1    inz(*Off)
014000040511     d datamsg         s             10
014100040511     d w1ccar          s                   like(v1ccar)
014200120306     d savsif          s                   like(knsif)
014300121016     d Autproj         s              1
014400140508
014500140508       //--------------------------------------------------------------
014600140508       //?Definizione aree dati.                                       ?
014700140508       //--------------------------------------------------------------
014800140508
014900140508       // -?Dati utente?
015000140508     d §AzUte        e ds                  extname(AZUTE00F)
015100140508     d                                     dtaara
015200140508     d §DatiUte      e ds                  extname(dDatiUte)
015300140508     d                                     dtaara
015400140508
015500140508       //--------------------------------------------------------------
015600140508       //?Definizione prototipi procedure.                             ?
015700140508       //--------------------------------------------------------------
015800140508
015900140508       // -?Reperimento dati utente?
016000140508     d TIBS34ds      e ds                  inz
016100140508      /copy gaitrasrc/srcProtoPR,TIBS34R
016200140508
016300121016     D*--------------------------------------------------
016400121016     D* Procedure name: GetProfiloUtenteProj
016500121016     D* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
016600121016     D* Returns:        Profilo utente in Proj.
016700121016     D* Parameter:      priProfiloUtente => Profilo Utente
016800121016     D*--------------------------------------------------
016900121016     D GetProfiloUtenteProj...
017000121016     D                 PR             1A
017100121016     D  priProfiloUtente...
017200121016     D                               10A
017300121016
017400000317      *------------------------------------------------------------------------*
017500000317      *---------------  C A L C O L O  ----------------------------------------*
017600000317      *------------------------------------------------------------------------*
017700000317
017800000320      * Emetto DSPF x richiesta parametri se non li ho ricevuti dal chiamante
017900000418     c                   IF        WI20vid <> 'N'
018000000317
018100000317      * Emissione VIDEO
018200000320     C     for01         tag
018300000317     C                   exsr      EMISd01
018400000317
018500000317      * f3=Fine
018600000317     c   kc              goto      fine
018700000317
018800000317      * f18=Cambio P.O. in gestione
018900000317     c   ks              eval      *in05 = *on
019000000317     c   ks              goto      for01
019100000317
019200000317      * 05 on --> controllo P.O. in gestione
019300000317     c     *in05         ifeq      *on
019400000317     C                   exsr      ctrfgs
019500000317     c   90              goto      for01
019600000317     c                   eval      *in05 = *off
019700000317     c                   end
019800000317
019900000317      * Controlli videata
020000000317     c                   exsr      ctrd01
020100000317
020200000317     C                   IF        *IN90 or *IN28 or *INKF = *off
020300020304     c                             or (*in09 and flagpo <> 'S')
020400000317     c                   goto      for01
020500000317     c                   ENDIF
020600090423
020700090417      * Imposto dati nella DS per scrivere FnRIG
020800000418     C                   IF        *INKF = *on
020900030321      *
021000030321      * Se rigenera per i DANNI e c'è un C/A già incassato, deve riattivare
021100030321      * la trasmissione del TNCSB sfleggando il flag e la data di trasmissione.
021200030321      *  e se di sede
021300030321     c                   if        V1Ccar = 'D' and
021400030321     c                             SIMFEL = *zeros
021500030321     c                   exsr      UPD_TNCSB
021600030321     c                   endIF
021700030321      *
021800000418     C                   clear                   TIBS20DS
021900000418     C                   eval      I20aas = V1Caas
022000000418     C                   eval      I20lnp = V1Clnp
022100000418     C                   eval      I20nrs = V1Cnrs
022200000418     C                   eval      I20nsp = V1Cnsp
022300000418     C                   eval      I20lna = V1Clna
022400000418     C                   eval      I20car = V1Ccar
022500000418     C                   eval      I20tra = 'E'
022600000418     C                   eval      I20por = V1Cfgs
022700020304
022800090423     c                   if        v1ccar = 'F' or v1ccar = 'S'
022900090423     c                   eval      i20por = v1cpor
023000090423     c                   eval      i20tra = 'N'
023100090423     c                   endif
023200020304
023300000418     C                   eval      I20pru = KNMUS
023400000418     C                   eval      I20dim = DATEU
023500000418     C                   movel     wora          I20him
023600000418     c                   ENDIF
023700090423
023800020131     c* per contrassegni omessi chiamo il pgm TNSC03R per immettere il
023900020131     c* contrassegno
024000121011     c                   if        v1ccar = '3' or v1ccar = '8'
024100020131     c                   movel(p)  tibs20ds      kpjbu
024200020131     c                   call      'TNSC03R'
024300020131     c                   parm                    kpjba
024400020131     c                   movel     kpjbu         tibs20ds
024500020131     c* se non convalido il contrassegno
024600020301     c                   if        o20err = '1'
024700020301     C                   eval      V1cMSG = o20msg
024800020301     c                   if        o20msg <> *blanks
024900020301     c                   seton                                        28
025000020301     c                   ENDIF
025100020131     c                   goto      for01
025200020131     c                   ENDIF
025300020131     c* chiamo pgm di stampa lettera per mittente/destinatario
025400121018     c                   if        v1ccar = '3'
025500020207     c                   call      'TNSC04C'
025600020131     c                   parm                    kpjba
025700020213     c                   parm                    OUTQA
025800020213     c                   parm                    OUTQP
025900121018     c                   endif
026000020131     c                   ENDIF
026100020131
026200000320     c                   ENDIF
026300000320
026400000418      * Scrivo file
026500000320     C                   exsr      Wrtfirig
026600121025     c                   movel     kpjbu         skpjbu
026700121025      * richiamo programmi per inserimento note
026800121025     c                   if        RigCau = '8' or RigCau = '3'
026900121025     c                   clear                   fidna4ds
027000121025     c                   if        RigCau = '8'
027100121025     c                   move      '1'           IA4MOD
027200121025     c                   else
027300121025     c                   move      '2'           IA4MOD
027400121025     c                   endif
027500121025      *
027600121025     c                   move      ' 30'         IA4MAD
027700121025     c                   move      'S'           IA4TOR
027800121025     c                   eval      IA4OGG = %editc(i20lnp: 'X') +
027900121025     c                             %editc(i20nrs: 'X') +
028000121025     c                             %editc(i20nsp: 'X') + %editc(i20aas: 'X')
028100121025     c                   eval      IA4NO2 = 'BOLLA RIGENERATA PER C/ASSEGNO -
028200121025     c                             OMESSO IL ' + (%editc(wdtgio:'Y'))
028300121025     c                   move      'TIBS20R'     IA4PGM
028400140508      * linea estero
028500121025     c                   if        §ogntw = 'DPD' or
028600121025     c                             §ogntw = 'FED' or
028700121025     c                             §ogntw = 'EEX'
028800140508     c                   clear                   fidnA6ds
028900140508     c                   eval      IDA06MAD = IA4MAD
029000140508     c                   eval      IDA06TOR = IA4TOR
029100140508     c                   eval      IDA06OGG = IA4OGG
029200140508     c                   eval      IDA06NO2 = IA4NO2
029300140508     c                   eval      iDA06ute = DUTute
029400140508     c                   eval      iDA06fil = DUTpou
029500140508     c                   call      'FIDNA6R'
029600140508     c                   parm                    fidnA6ds
029700121025      * linea italia
029800121025     c                   else
029900121025     c                   movel     fidna4ds      kpjbu
030000121025     c                   call      'FIDNA4R'
030100121025     c                   parm                    kpjba
030200121025     c                   endif
030300121025     c                   endif
030400121025     c                   movel     skpjbu        kpjbu
030500000418      * Riemetto il formato
030600000420     c                   IF        WI20vid <> 'N'
030700000320     C                   clear                   V1Caas
030800000320     C                   clear                   V1Clnp
030900000320     C                   clear                   V1Cnrs
031000000320     C                   clear                   V1Cnsp
031100000320     C                   clear                   V1Clna
031200000320     C                   clear                   V1Dlna
031300000320     C                   clear                   V1Ccar
031400020304     c                   clear                   v1cpor
031500020304     c                   clear                   v1dpor
031600020903     c                   Clear                   V1cccm
031700020903     c                   Clear                   V1cccd
031800020903     c                   Clear                   V1dccm
031900020903     c                   Clear                   V1dccd
032000020904     c                   Movel     'M'           V1fmd
032100040510     c                   Eval      v1cinv = 'SI'
032200040510     c                   Eval      v1ctar = 'SI'
032300040511     c                   Eval      wforza = *Off
032400000320     C                   eval      *in28 = *on
032500000320     c                   eval      v1cmsg = msg(3)
032600000320     c                   goto      for01
032700000420     c                   ENDIF
032800000317
032900000317     c     fine          tag
033000120306     c                   eval      savsif=knsif
033100000317
033200000418      * Elaboro record scritti
033300000320     C                   SELECT
033400000321     C                   WHEN      wrec = *zeros
033500000418     C                   WHEN      SIMFEL = *zeros
033600120306     c                   if        %subst(knsif:7:1)='P'
033700120306     c                   eval      knsif='GAITRAPSP '
033800120306     c                   else
033900120306     c                   eval      knsif='GAITRA201 '
034000120306     c                   endif
034100120306
034200040510     c                   If        wi20vid <> 'S'
034300121022     c                   if        RigCau = '8'
034400121018      *forza scrittura su file spia e richiama programma di ricezione del rekord
034500121018      *in modo forzato
034600121018     c                   exsr      ficso
034700121018     c                   movel     §membro       mbrfil
034800121018     c                   eval      kcoaz = 'LRO2'
034900121018     c                   call      'BCH10'
035000121018     c                   parm                    kpjba
035100121018     c                   else
035200000418     C                   clear                   KPJBU
035300000418     c                   eval      kcoaz = 'BS21'
035400020827     c                   call      'BCH10'
035500000418     c                   parm                    kpjba
035600121018     c                   endif
035700040510     c                   Else
035800040510     C                   clear                   KPJBU
035900040510     c                   eval      kcoaz = 'BS25'
036000040513     c                   call      'BCH10'
036100040510     c                   parm                    kpjba
036200040510     c                   EndIf
036300121022     c                   ENDSL
036400121022     c                   eval      knsif=savsif
036500121022
036600000317     c                   EVAL      *INLR = *ON
036700030321      **********************************************************************
036800030321      * Solo per i DANNI aggiorna
036900030321      **********************************************************************
037000030321     C     UPD_TNCSB     BegSR
037100030321      *
037200030321      * se Trovato un C/A e già incassato deve sflaggare per essere
037300030321      *  ritrasmesso.
037400030321     C     kspe          chain     tncsb03L
037500030321     c                   if        %found(tncsb03l)
037600030321      *  se già incassato:data arrivo distinta incasso <> 0
037700030321     c                   if        csbdta >0
037800030321     c                   move      *blank        csbftr
037900030321     c                   move      *zeros        csbdtr
038000030321     c                   end
038100030321     C                   update    tncsb000
038200030321     c                   end
038300030321      *
038400030321     C                   EndSR
038500000317      **********************************************************************
038600000317      * EMISSIONE VIDEO 1
038700000317      **********************************************************************
038800000317     C     EMISD01       BEGSR
038900000317
039000040510     c                   Select
039100040510     c                   When      WI20vid = 'W'
039200000417     c                   exfmt     BS20W01
039300040510     c                   When      WI20vid = 'S'
039400040510     c                   exfmt     BS20W02
039500040510     C                   Other
039600000317     c                   write     BS20T01
039700000317     c                   write     BS20Z01
039800000417     c                   exfmt     BS20D01
039900040510     c                   ENDsl
040000000317
040100000317      * Azzero Campi ed Indicatori
040200000320     c                   eval      *IN41 = *OFF
040300000320     c                   eval      *IN42 = *OFF
040400000418     c                   eval      *IN43 = *OFF
040500020304     c                   eval      *in44 = *off
040600020903     c                   Eval      *In45 = *off
040700000317     c                   eval      *IN28 = *off
040800000317     c                   eval      *IN90 = *off
040900000317     c                   clear                   v1cmsg
041000020304
041100020903      * se ho riemesso la videata per *in09 imposto flag
041200020304     c                   if        *in09
041300020304     c                   eval      flagpo = 'S'
041400020304     c                   else
041500020304     c                   clear                   flagpo
041600020304     c                   endif
041700020304
041800000317
041900000317     C                   ENDSR
042000000317      **********************************************************************
042100000317      * CONTROLLO IL P.O. IN IN GESTIONE
042200000317      **********************************************************************
042300000317     C     CTRFGS        beGSR
042400000317      *
042500000320     c                   eval      *in40 = *off
042600000317     c                   eval      *in90 = *off
042700000317     c                   clear                   v1dfgs
042800000317     C                   clear                   FNLV50DS
042900000317     C                   movel     KNMUS         D50PRU
043000000317     C                   movel     V1cfgs        D50FGS
043100000317      *
043200000317     C                   CALL      'FNLV50R'
043300000317     C                   PARM                    FNLV50DS
043400000317      *
043500000317     C                   IF        D50ERR <> *blanks
043600000317     C                   movel     D50MSG        V1cMSG
043700000317     c                   eval      *in28 = *on
043800000317     c                   eval      *in40 = *on
043900000317     c                   eval      *in90 = *on
044000000317     C                   else
044100000317      *
044200000317     C                   z-add     v1cfgs        Wfgs
044300000317     C                   clear                   L6
044400000317     C                   EXSR      CARL6
044500000317     C                   movea     LIN           L6
044600000317      *
044700000317      * decodifica
044800000317     c     v1cfgs        CHAIN     azorg
044900000317     c                   IF        %FOUND
045000000317     C                   movel     orgDES        V1DFGS
045100000317    2C                   ENDIF
045200000317      *
045300000317     C                   ENDIF
045400000317      *
045500000317     c                   ENDSR
045600000317      **********************************************************************
045700000317      * CONTROLLI VIDEO 1
045800000317      **********************************************************************
045900000317     C     Ctrd01        BEGSR
046000020305
046100000317      *
046200000317      * Controllo linea di partenza
046300000317     C                   IF        V1Clnp = *zeros
046400000320     c                   MOVEL     MSG(1)        v1CMSG
046500000317     c                   eval      *in28 = *on
046600000317     c                   ELSE
046700000317      *
046800000317     c     V1Clnp        chain     azorg01l
046900000317     c                   If        not %Found  OR  orgfva <> *blanks
047000000320     c                   movel     MSG(2)        v1CMSG
047100000317     c                   eval      *in28 = *on
047200000317     c                   Endif
047300000317     c                   ENDIF
047400000317      *
047500000317     c                   IF        *IN28 = *ON
047600000320     c                   eval        *IN41 = *on
047700000317     c                   goto      ev1spe
047800000317     C                   ENDIF
047900021014      * reperisce outq della linea di partenza
048000021014     c                   z-add     v1clnp        codpo
048100021014     c                   exsr      sroutq
048200021014     c                   movel     outq          outqp
048300000317      *   sistemo anno di due cifre
048400000317     C                   If        V1Caas < 100  and V1Caas > 60
048500000317     C                   add       1900          V1Caas
048600000317     C                   Endif
048700000317     C                   If        V1Caas < 100  and V1Caas <= 60
048800000317     C                   add       2000          V1Caas
048900000317     C                   Endif
049000000317      *  Controllo key spedizione
049100020221     c                   IF        V1clnp = *zeros  or  V1Cnsp = *zeros
049200020221     c                   eval        *IN28 = *on
049300000320     c                   movel     MSG(4)        v1CMSG
049400000317     c                   goto      ev1spe
049500000317     C                   ENDIF
049600000317      *
049700000317      * Controllo linea di arrivo
049800000317     C                   IF        V1Clna = *zeros
049900000320     c                   movel     MSG(1)        v1CMSG
050000000317     c                   eval      *in28 = *on
050100000317     c                   ELSE
050200000317      *
050300000317     c     V1Clna        chain     azorg01l
050400000317     c                   If        not %Found  OR  orgfva <> *blanks
050500000320     c                   movel     MSG(2)        v1CMSG
050600000317     c                   eval      *in28 = *on
050700000317     c                   Endif
050800040331     c* se linea arrivo DPD e rigeneraz. x contrassegni omessi errore
050900040331     c                   If        %Found and  orgfva = *blanks
051000121011     c                             and (V1Ccar = '3' or v1ccar = '8')
051100040331     c                   MOVEL     ORGDE3        OG143
051200121023     c                   select
051300121023     c                   when      §ogntw = 'DPD' or
051400121023     c                             §ogntw = 'FED'
051500040331     c                   movel     MSG(21)       v1CMSG
051600040331     c                   eval      *in28 = *on
051700170914     c**                 when      §ogntw = 'EEX' and v1ccar = '8'
051800170914     c**                 movel     MSG(10)       v1CMSG
051900170914     c**                 eval      *in28 = *on
052000121023     c                   Endsl
052100040331     c                   Endif
052200000317     c                   ENDIF
052300000317      *
052400000317     c                   IF        *IN28 = *ON
052500000320     c                   eval        *IN42 = *on
052600000317     c                   goto      ev1spe
052700000317     C                   ENDIF
052800021014     c* reperisce outq della linea di arrivo
052900021014     c                   z-add     v1clna        codpo
053000021014     c                   exsr      sroutq
053100021014     c                   movel     outq          outqa
053200000418      * Controllo causale richiesta
053300121011     c                   IF        V1Ccar <> 'D'
053400040510     c                             and v1ccar <> 'F' and v1ccar <> 'S'
053500121011     c                             and v1ccar <> '3' and v1ccar <> '8'
053600000418     c                   movel     MSG(10)       v1CMSG
053700000418     c                   eval      *in28 = *on
053800000418     c                   eval      *IN43 = *on
053900000418     c                   goto      ev1spe
054000000418     C                   ENDIF
054100040510
054200040510s   1c                   Select
054300020204      * la causale contrassegni omessi ('C') è abilitata solo:
054400121019      * se si è in sede    le causali sono diventate 3 e 8 non più C
054500121011w   1c                   when      V1Ccar = '3' or v1ccar = '8'
054600020201     c                   if        simfel <> *zeros
054700020201     c                   movel     MSG(14)       v1CMSG
054800020201     c                   eval      *in28 = *on
054900020201     c                   eval      *IN43 = *on
055000020201     c                   goto      ev1spe
055100020201     C                   ENDIF
055200020212      * se l'utente è 'FILxxx' oppure 'CON*' oppure 'EDP*'
055300121016     c                   if        Autproj <> 'S'
055400121016     c*m                 if        %subst(knmus: 1: 3) <> 'FIL' and
055500121016     c*m                           %subst(knmus: 1: 3) <> 'CON' and
055600121016     c*m                           %subst(knmus: 1: 3) <> 'EDP'
055700020201     c                   movel     MSG(15)       v1CMSG
055800020201     c                   eval      *in28 = *on
055900020201     c                   eval      *IN43 = *on
056000020201     c                   goto      ev1spe
056100020204     c                   end
056200020206     c                   move      v1clnp        com03             3
056300140508     c                   if        DUTpou <> 046
056400140508     c                   if        v1clnp <> DUTpou
056500121016     c*m                 if        %subst(knmus: 1: 3) =  'FIL' and
056600121016     c*m                           %subst(knmus: 4: 3) <> com03
056700020204     c                   movel     MSG(18)       v1CMSG
056800020204     c                   eval      *in28 = *on
056900020204     c                   eval      *IN43 = *on
057000020204     c                   goto      ev1spe
057100020201     C                   ENDIF
057200121016     C                   Endif
057300040510
057400020304      * la causale bolla firmata ('F') è abilitata solo:
057500020304      * se si è in sede
057600040510w   1c                   When      V1Ccar = 'F'
057700040510     c                   if        simfel <> *zeros
057800020304     c                   movel     MSG(22)       v1CMSG
057900020304     c                   eval      *in28 = *on
058000020304     c                   eval      *IN43 = *on
058100020304     c                   goto      ev1spe
058200020304     C                   endif
058300040510
058400040511      * Se richiesta bolla firmata scanner ('S')
058500040510w   1c                   When      v1ccar = 'S'
058600040510      * - devo riemettere la videata se richiedono l'invio della bolla rigenerata al cliente
058700040510if  2c                   If        flagpo <> 'S'
058800040510     c                   If        v1cinv = 'SI'
058900040510     c                   Eval      *In09 = *On
059000040510     c                   EndIf
059100040510      * - se no esco dal pgm
059200040510     c                   If        v1cinv = 'NO'
059300040510     c                   Goto      fine
059400040510     c                   EndIf
059500040510e   2c                   EndIf
059600040510e   1c                   EndSl
059700040510
059800000418      * Se sono in SEDE controllo che:
059900000418     C                   IF        SIMFEL = *zeros
060000000317     C     kspe          chain     titas30c
060100000418      *
060200000418     c                   SELECT
060300000418      *  la spedizione deve esistere
060400000418     c                   WHEN      not %found(titas30c)
060500000418     c                   movel     MSG(5)        v1CMSG
060600000317     c                   eval      *in28 = *on
060700000418      *  la spedizione non deve essere antecedente al settembre 1999
060800000418     c                   WHEN      TASaas < 1999  or
060900000418     c                             (TASaas = 1999  and  TASmgs < 0901)
061000000418     c                   movel     MSG(7)        v1CMSG
061100000418     c                   eval      *in28 = *on
061200040510
061300020204      * se la causale è "C/A omesso"
061400121011     C                   WHEN      V1Ccar = '3' or v1ccar = '8'
061500020201      * la bolla non deve essere POSTA
061600020225     c                   if        §ogntw = 'PPT'
061700020201     c                   movel     MSG(16)       v1CMSG
061800020201     c                   eval      *in28 = *on
061900020201     c                   eval      *IN43 = *on
062000020201     c                   goto      ev1spe
062100020201     C                   ENDIF
062200020204      * la bolla non deve essere AUTOMATICA solo per utenti FIL*
062300140508     c                   if        DUTpou <> 046
062400121016     c*m                 if        %subst(knmus: 1: 3) =  'FIL'
062500020703     c                   move      '3M'          tblcod
062600020201     c                   movel(p)  tassop        tblkey
062700020201     c     ktab          setll     tabel00f                               28
062800020201     c                   if        *in28
062900020201     c                   movel     MSG(17)       v1CMSG
063000020201     c                   eval      *IN43 = *on
063100020201     c                   goto      ev1spe
063200020201     C                   ENDIF
063300020204     C                   ENDIF
063400020201      *  il C/A non deve esistere                                      annullato
063500000417     C     kspe          setll     tncsb000
063600000417     c                   If        %equal
063700000418     c                   movel     MSG(6)        v1CMSG
063800000417     c                   eval      *in28 = *on
063900020201     c                   goto      ev1spe
064000000417     c                   Endif
064100040510
064200020305      * Se causale è Bolla Firmata
064300020305     c                   when      v1ccar = 'F'
064400040511     c                   Eval      dtasflo = tasflo
064500020305      * la spedizione deve essere consegnata
064600041118     c                   if        tasndc = *zeros or TasDcm = *Zeros
064700020305     c                   eval      v1cmsg = msg(24)
064800020305     c                   eval      *in28 = *on
064900020305     c                   goto      ev1spe
065000020305     c                   endif
065100020507      * la spedizione non deve essere più vecchia di un anno
065200020507     c                   movel     tasaas        wtasdta
065300020507     c                   move      tasmgs        wtasdta
065400020507     c                   if        wtasdta < wolddata
065500020507     c                   eval      v1cmsg = msg(25)
065600020507     c                   eval      *in28 = *on
065700020507     c                   goto      ev1spe
065800020507     c                   endif
065900000418     C                   ENDSL
066000020703
066100020703      *  la spedizione non deve essere una bolla di recupero
066200020703     c                   clear                   ds3a
066300020703     c                   move      '3A'          tblcod
066400020703     c                   movel(p)  tascbo        tblkey
066500020703     c     ktab          chain     tabel00f
066600020703     c                   if        %found(tabel00f) and tblflg = *blanks
066700020703     c                   movel     tbluni        ds3a
066800020703     c                   if        §3arbl = 'R'
066900040511     c                             and V1ccar <> 'S'
067000020703     c                   eval      *in28 = *on
067100020703     c                   eval      v1cmsg = msg(26)
067200020703     c                   goto      ev1spe
067300020703     c                   endif
067400020703     c                   endif
067500020903
067600020903      * Se richiesta Bolla Firmata imposto il mittente e il destinatario in base al tipo bolla
067700040510      * anche se bolla firmata da scanner
067800020903 b1  c                   If        V1ccar = 'F'
067900040510     c                             or V1ccar = 'S'
068000020903     c                   Movel     §3atb1        Wtb1
068100020903 b2  c                   If        Wtb1 = 'F'
068200020903     c                   Movel     Tasccm        V1cccm
068300020903     c                   Movel     V1clna        V1cccd
068400020903     c                   Move      9999          V1cccd
068500020903 e2  c                   EndIf
068600020903 b2  c                   If        Wtb1 = 'A'
068700020903     c                   Movel     Tasccm        V1cccm
068800020903     c                   Movel     TasKsc        V1cccd
068900020903 e2  c                   EndIf
069000020903
069100020903     c                   Move      V1cccm        Wccm
069200020903     c                   Move      V1cccd        Wccd
069300020903     c                   Clear                   V1dccm
069400020903     c                   Clear                   V1dccd
069500020903      * Recupero le ragioni sociali
069600020903      * Da titaa se mittente non codificato
069700020903 b2  c                   If        Wccm = 8888 or Wccm = 9999
069800020903     c                   Eval      Wtrc = 'M'
069900020903     c     Kspea         Chain     Titaa30c
070000020903 b3  c                   If        %Found(Titaa30c)
070100020903     c                   Movel     Taarsc        V1dccm
070200020903 e3  c                   EndIf
070300020903 x2  c                   Else
070400020903      * Da Piano dei conti se mittente codificato
070500020903     c                   clear                   TIBS69DS
070600020903     c                   Movel     V1cccm        I69kac
070700020903     c                   call      'TIBS69R'
070800020903     c                   parm                    tibs69DS
070900020903     c                   parm                    ds_cnaco
071000020903     c                   parm                    ds_cnind
071100020903     c                   parm                    ds_cnclp
071200020903     c                   parm                    ds_fncls
071300020903 b3  c                   if        o69err <> '1'
071400020903     c                   movel     acorag        V1dccm
071500020903 e3  c                   EndIf
071600020903 e2  c                   EndIf
071700020903      * Da titas se destinatario non codificato
071800020903 b2  c                   If        Wccd = 8888 or Wccd = 9999
071900020903     c                   Movel     Tasrsd        V1dccd
072000020903 x2  c                   Else
072100020903     c                   clear                   TIBS69DS
072200020903     c                   Movel     V1cccd        I69kac
072300020903     c                   call      'TIBS69R'
072400020903     c                   parm                    tibs69DS
072500020903     c                   parm                    ds_cnaco
072600020903     c                   parm                    ds_cnind
072700020903     c                   parm                    ds_cnclp
072800020903     c                   parm                    ds_fncls
072900020903 b3  c                   if        o69err <> '1'
073000020903     c                   movel     acorag        V1dccd
073100020903 e3  c                   EndIf
073200020903 e2  c                   EndIf
073300020903 e1  c                   EndIf
073400000418      *
073500000418      * Controllo Legami Bolla
073600000418     c                   exsr      legbol
073700000418     C                   SELECT
073800000419      *   Se causale C/A omessi non elaboro le bolle legate
073900121011     C                   WHEN      wlegame <> *blanks and
074000121011     C                             (V1Ccar = '3'  or v1ccar = '8')
074100000418     c                   movel     MSG(11)       v1CMSG
074200000418     c                   eval      *in28 = *on
074300000419      *   Se causale Danni elaboro solo mamma originale o ultima figlia
074400000418     C                   WHEN      V1Ccar = 'D'  and  wlegame = 'X'
074500000418     c                   movel     MSG(12)       v1CMSG
074600000418     c                   eval      *in28 = *on
074700020304      *   Se causale Bolla firmata elaboro solo mamma originale o ultima figlia
074800020304     C                   WHEN      V1Ccar = 'F'  and  wlegame = 'X'
074900020304     c                   movel     MSG(19)       v1CMSG
075000020304     c                   eval      *in28 = *on
075100000419      *   Controllo se la mamma originale rientra nei limiti di data 9/99
075200000419     C                   WHEN      Wlegame = 'S'  and  (LBLaao < 1999  or
075300000419     c                                  (LBLaao = 1999  and  LBLmgo < 0901))
075400000419     c                   movel     MSG(13)       v1CMSG
075500000419     c                   eval      *in28 = *on
075600000418     C                   ENDSL
075700020304
075800020304      * Se richiesta Bolla Firmata devo riemettere la videata per dare la
075900020304      * possibilità di inserire il p.o. che effettua la richiesta.
076000040510     c                   if        v1ccar = 'F' or v1ccar = 'S'
076100020304      * se già riemessa la videata
076200020304     c                   if        flagpo = 'S'
076300020903      * La bolla firmata può essere rigenerata solo se il mittente o il destinatario sono
076400020903      * dei clienti codificati
076500020903     c                   If        (Wccm = 8888 or Wccm = 9999) and
076600020903     c                             (Wccd = 8888 or Wccd = 9999)
076700020903     c                   Movel     Msg(27)       V1cmsg
076800020903     c                   Eval      *In28 = *On
076900020903     c                   Eval      *In41 = *On
077000020903     c                   GoTo      Ev1spe
077100020903     c                   EndIf
077200020903      * Immettere il flag di addebito
077300020903     c                   If        V1fmd = *blanks
077400020903     c                   Movel     Msg(30)       V1cmsg
077500020903     c                   Eval      *In28 = *On
077600020903     c                   Eval      *In45 = *On
077700020903     c                   GoTo      Ev1spe
077800020903     c                   EndIf
077900020903      * Se la bolla va addebitata al mittente  deve essere un codificato
078000020903     c                   If        V1fmd = 'M' and
078100020903     c                             (Wccm = 8888 or Wccm = 9999)
078200020903     c                   Movel     Msg(28)       V1cmsg
078300020903     c                   Eval      *In28 = *On
078400020903     c                   Eval      *In45 = *On
078500020903     c                   GoTo      Ev1spe
078600020903     c                   EndIf
078700020903      * Se la bolla va addebitata al destinatario  deve essere un codificato
078800020903     c                   If        V1fmd = 'D' and
078900020903     c                             (Wccd = 8888 or Wccd = 9999)
079000020903     c                   Movel     Msg(29)       V1cmsg
079100020903     c                   Eval      *In28 = *On
079200020903     c                   Eval      *In45 = *On
079300020903     c                   GoTo      Ev1spe
079400020903     c                   EndIf
079500040510      * controllo che non sia stata già fatta una richiesta di rigenerazione per questa bolla
079600040510      * se bolla firmata scanner
079700040510if  3c                   If        v1ccar = 'S'
079800090417     c     kspe          Setll     Fnrig02l
079900040510     c                   Do        *Hival
080000090417     c     kspe          Reade     Fnrig02l
080100090417     c                   If        %Eof(Fnrig02l)
080200040510     c                   Leave
080300040510     c                   EndIf
080400040511     c                   Clear                   w1ccar
080500091202     c                   if        rigcau = v1ccar
080600040511     c                   Select
080700091202     c                   when      v1ctar = 'SI' and V1fmd = 'M' and
080800091202     c                             rigadd = 'M' and rigtas = 'T'
080900091202     c                   Eval      w1ccar = 'S'
081000091202     c                   when      v1ctar = 'SI' and V1fmd = 'D' and
081100091202     c                             rigadd = 'D' and rigtas = 'T'
081200091202     c                   Eval      w1ccar = 'S'
081300091202     c                   when      v1ctar = 'NO' and V1fmd = 'M' and
081400091202     c                             rigadd = 'M' and rigtas = 'N'
081500091202     c                   Eval      w1ccar = 'S'
081600091202     c                   when      v1ctar = 'NO' and V1fmd = 'D' and
081700091202     c                             rigadd = 'D' and rigtas = 'N'
081800091202     c                   Eval      w1ccar = 'S'
081900040511     c                   EndSl
082000040510      * controllo se dello stesso tipo
082100090417     c                   If        rigcau = w1ccar and wforza = *Off
082200090423     c                   Movel     rigdata       dtaimm
082300040510     c                   Eval      *In28 = *On
082400040511     c                   Eval      v1cmsg = msg(33)
082500040511     c                   Eval      datamsg = %trim (giorno + '/' +
082600040511     c                             mese + '/' + anno)
082700040511     c                   Eval      %subst(v1cmsg:30:10) = datamsg
082800040510     c                   Eval      wforza = *On
082900040510     c                   Goto      ev1spe
083000040510     c                   EndIf
083100091202     c                   endif
083200040511     c                   EndDo
083300040510    3c                   EndIf
083400020903
083500020304      * P.O. obbligatorio
083600020304     c                   if        v1cpor = *zeros
083700020304     c                   movel     msg(20)       v1cmsg
083800020304     c                   eval      *in28 = *on
083900020304     c                   eval      *in44 = *on
084000020304     c                   goto      ev1spe
084100020304     c                   endif
084200020304      * Controllo p.o. immesso
084300020304     c     v1cpor        chain     azorg01l
084400020304     c                   if        not %found(azorg01l) or orgfva <> *blanks or
084500020304     c                             (orgfag <> 'F' and orgfag <> 'A')
084600020304     c                   movel     msg(21)       v1cmsg
084700020304     c                   eval      *in28 = *on
084800020304     c                   eval      *in44 = *on
084900020304     c                   goto      ev1spe
085000020304     c                   endif
085100020304     c                   movel     orgdes        v1dpor
085200040510      * se bolla firmata e il p.o. che effettua la richiesta è abilitato alla scannerizzazione
085300040511      * e esiste l'immagine della LDV errore bloccante
085400040510     c                   Eval      Og146 = orgde6
085500040511     c                   If        §Ogdse > *Zeros and v1ccar = 'F' and
085600040511     c                             §floiml <> *Blanks
085700040510     c                   Eval      v1cmsg = msg(31)
085800040510     c                   Eval      *In28 = *On
085900040510     c                   Eval      *In44 = *On
086000040510     c                   Goto      ev1spe
086100040510     c                   EndIf
086200040510      * se bolla firmata da scanner e il p.o. che effettua la richiesta non è abilitato alla
086300040510      * scannerizzazione errore bloccante
086400040511     c                   If        §Ogdse = *Blanks and v1ccar = 'S'
086500040510     c                   Eval      v1cmsg = msg(32)
086600040510     c                   Eval      *In28 = *On
086700040510     c                   Eval      *In44 = *On
086800040510     c                   Goto      ev1spe
086900040510     c                   EndIf
087000020304     c                   else
087100020304      * non è stata ancora riemessa la videata
087200020304     c                   eval      *in09 = *on
087300040512     c                   If        v1ccar <> 'S'
087400020903     c                   Eval      *In45 = *On
087500040512     c                   EndIf
087600020304     c                   endif
087700020304     c                   endif
087800020304
087900000418      *
088000000418     C                   ELSE
088100000418      * Se sono in FILIALE controllo:
088200000418     C     kspe          chain     fnarb000
088300121019     c                   if        NOT %found  and
088400121019     c                             (V1Ccar <> '3' or v1ccar <> '8')
088500000418     C     kspe          chain     fnblp000
088600000418     c                   endif
088700000418      *  Che la spedizione non esista
088800000418     c                   If        %found
088900000418     c                   movel     MSG(8)        v1CMSG
089000000418     c                   eval      *in28 = *on
089100000418     c                   Else
089200000418      *  Che sia gestibile
089300000418     c     V1Clnp        lookup    l6                                     30
089400000418     c  n30V1Clna        lookup    l6                                     30
089500000418     c                   If        *in30 = *off
089600000418     c                   movel     MSG(9)        v1CMSG
089700000418     c                   eval      *in28 = *on
089800000418     c                   Endif
089900000418     c                   Endif
090000000418      *
090100000317     c                   ENDIF
090200000317      *
090300000321     c                   If        *in28 = *on
090400000320     c                   eval      *in41 = *on
090500000418     c                   goto      ev1spe
090600000317     c                   Endif
090700000317      *
090800000317     c     ev1spe        ENDSR
090900000320      **********************************************************************
091000000418      * DETERMINO LEGAMI BOLLA
091100000320      **********************************************************************
091200000418     C     LEGbol        BEGSR
091300000320      *
091400000418     C                   clear                   wlegame
091500000418      * Non considero i resi come bolle legate se causale richiesta è DANNI
091600110218    3c**                 IF        TASfbr = *blanks  or                         * bolla di reso
091700110218    3c**                           (TASfbr = 'S' and V1Ccar <> 'D')             * bolla di reso
091800110218     c
091900110218     c                   if        v1ccar<>'D' or (tascca<>'6' and tascca<>'2'
092000110218     c                             and tasfbr<>'S' )
092100000418      *
092200000418     c     kspe          chain     fnlbl01l
092300000418     c     Kspe          chain     fnlbl02l
092400000418      *
092500000418     C                   SELECT
092600000418      * Nessun legame
092700000418    3c                   WHEN      NOT %found(fnlbl01l)  and
092800000418    3c                             NOT %found(fnlbl02l)
092900000418      * La spedizione è mamma e figlia
093000000418    3c                   WHEN      %found(fnlbl01l)  and  %found(fnlbl02l)
093100000418     c                   eval      Wlegame = 'X'
093200000418      * La spedizione è mamma originale o ultima figlia
093300000418    3c                   OTHER
093400000418     c                   eval      Wlegame = 'S'
093500000418    1c                   ENDSL
093600000418      *
093700000418     C                   ENDIF
093800000418      *
093900000418     c                   ENDSR
094000000418      **********************************************************************
094100090417      * SCRIVO FILE FNRIG
094200000418      **********************************************************************
094300000418     C     WRTFIRIG      BEGSR
094400000418      *
094500090417     C                   clear                   FNRIG000
094600000320     C                   eval      RIGaas = I20aas
094700000320     C                   eval      RIGlnp = I20lnp
094800000320     C                   eval      RIGnrs = I20nrs
094900000418     C                   eval      RIGnsp = I20nsp
095000000320     C                   eval      RIGlna = I20lna
095100090423      * solo per le bolle firmate che generano bolla di addebito
095200090423     c                   if        i20car = 'F' or i20car = 'S'
095300090423      * imposto il ksc
095400090423     c                   if        v1fmd = 'M'
095500090423     c                   eval      rigksc = v1cccm
095600090423     c                   endif
095700090423     c                   if        v1fmd = 'D'
095800090423     c                   eval      rigksc = v1cccd
095900090423     c                   endif
096000090423      * addebito a carico mittente o destinatario
096100090423     c                   eval      rigadd = v1fmd
096200090423      * se tassazione da tariffa o negata
096300090423      * la scelta è prevista solo per le bolle 'FF'
096400090423      * quindi per le 'FO' imposto fisso da tariffa
096500090423     c                   select
096600090423     c                   when      i20car = 'F' or v1ctar = 'SI'
096700090423     c                   eval      rigtas = 'T'
096800090423     c                   when      v1ctar = 'NO'
096900090423     c                   eval      rigtas = 'N'
097000090423     c                   endsl
097100090423      * n. colli
097200090423     c                   eval      rigncl = 1
097300090423      * codice tariffa
097400090423      * lo imposto se:
097500090423      * bolla franco e paga mittente o se bolla assegnato e paga destinatario
097600090423     c                   if        (v1fmd = 'M' and wtb1 = 'F') or
097700090423     c                             (v1fmd = 'D' and wtb1 = 'A')
097800090423     c                   move      tasctr        rigctr
097900090423     c                   endif
098000090423     c                   endif
098100090423
098200090417     C                   eval      RIGcau = I20car
098300000320     C                   eval      RIGtra = I20tra
098400090417     C                   eval      RIGfil = I20por
098500000320     C                   eval      RIGpru = I20pru
098600090417     C                   eval      RIGdata = I20dim
098700121011     C                   movel     o20tinc       RIGflo
098800090423     C                   movel     wora          RIGora
098900121018     c                   if        v1ccar = '8'
099000121018     C                   move      dateu         RIGdela
099100121018     c                   endif
099200000320      *
099300090417     c                   write     FNRIG000
099400000321     C                   add       1             wrec
099500000320      *
099600000320     c                   ENDSR
099700000317      **************************************************************************
099800000317      * CARICO SCHIERA L6 DA TABELLA £6
099900000317      **************************************************************************
100000000317     C     carl6         BEGSR
100100000317      *
100200000317     C                   CLEAR                   TRUL06DS
100300000317     C                   MOVE      '£6'          D06COD
100400000317     C                   MOVEL(P)  Wfgs          D06key
100500000317     C                   MOVEL(P)  TRUL06DS      KPJBU
100600000317     C                   CALL      'TRUL06R'
100700000317     C                   PARM                    KPJBA
100800000317     C                   MOVEL     KPJBU         TRUL06DS
100900000317      *
101000000317     c                   endsr
101100121018      **************************************************************************
101200121018      * forza scrittura spia
101300121018      **************************************************************************
101400121018     C     ficso         BEGSR
101500121018      *
101600121022     C                   move      riglnp        §fle
101700121018      *richiama override per membro
101800121018     c                   exsr      ovrficso
101900121018      * scrive
102000121019     c                   open      ficso00r
102100121018     C                   clear                   FICSO000
102200121018     C                   MOVEL     'CRSE'        csoCAV
102300121018     C                   Z-ADD     dateu         csoDTV
102400121019     C                   TIME                    csoORV
102500121019     C                   Z-ADD     i20lnp        csoLNP
102600121019     C                   Z-ADD     i20lna        csoLNA
102700121019     C                   Z-ADD     i20lna        csoFEV
102800121019     C                   Z-ADD     i20aas        csoAAS
102900121019     C                   Z-ADD     i20nrs        csoNRS
103000121019     C                   Z-ADD     i20nsp        csoNSP
103100121018     C                   MOVEL     '8'           csoSTA
103200121018     c                   write     FICSO000                             59
103300121019     c                   close     ficso00R
103400121018     c                   endsr
103500121018     C**-------------------------------------------------------------**
103600121018     C** ovrdbf membri  FICSO00r
103700121018     C**-------------------------------------------------------------**
103800121018     C     ovrficso      begsr
103900121018     C** Controllo se esiste membro
104000121018     C                   z-add     49            lung             15 5
104100121019     C                   movea     cmd3          comman           80
104200121018     c                   eval      %subst(comman:43:6) = §mbr
104300121018     C                   CALL      'QCMDEXC'                            34
104400121018     C                   PARM                    comman
104500121018     C                   PARM                    lung
104600121018     C** Se non esiste eseguo addpfm x membro nuovo
104700121018     C                   if        *in34
104800121018     C                   movea     cmd1          comman
104900121018     c                   eval      %subst(comman:43:6) = §mbr
105000121018     C                   call      'QCMDEXC'
105100121018     C                   parm                    comman
105200121018     C                   parm                    lung
105300121018     C                   end
105400121018     C** Esegue in ogni caso OVRDBF del membro desiderato
105500121018     C                   movea     cmd2          comman
105600121018     c                   eval      %subst(comman:43:6) = §mbr
105700121018     C                   CALL      'QCMDEXC'
105800121018     C                   parm                    comman
105900121018     C                   parm                    lung
106000121018     C*
106100121018     C                   endsr
106200000317      *****************************************************************
106300021014      * reperisce outq
106400000317      *****************************************************************
106500021014     C     sroutq        BEGSR
106600021014     c* reperisce il nome della coda di stampa
106700021014     c                   move      'L'           finep
106800021014     c                   move      '0'           tpcoda
106900021014     c                   clear                   outq
107000021014     c                   clear                   libcoda
107100021014     c                   clear                   esito
107200021014     c                   call      'TRULOUTQ'
107300021014     c                   parm                    finep
107400021014     c                   parm                    codpo
107500021014     c                   parm                    tpcoda
107600021014     c                   parm                    outq
107700021014     c                   parm                    libcoda
107800021014     c                   parm                    esito
107900021014     c                   if        esito <> '0' or outq = *blank
108000021014     c                   eval      outq = 'QDKT'
108100021014     c                   endif
108200021014      *
108300021014     c                   endsr
108400021014      *****************************************************************
108500021014      * ROUTINE INIZIALE
108600021014      *****************************************************************
108700021014     C     *INZSR        BEGSR
108800000317      *
108900000317     C     *ENTRY        PLIST
109000000317     C                   PARM                    KPJBA
109100000317      *
109200000320     C                   clear                   TIBS20DS
109300000320      *
109400000418      *  Gestione parametri ricevuti
109500000418     C                   SELECT
109600000418      *  Se non emetto videate ricevo la DS impostata
109700000418     C                   WHEN      %subst(Kpjbu:1:1) = 'N'
109800000320     C                   movel     kpjbu         TIBS20DS
109900000418     c                   eval      Wi20vid = I20vid
110000000418      *  Se emetto la window ricevo la DS impostata (key sped + lna)
110100000418     C                   WHEN      %subst(Kpjbu:1:1) = 'W'
110200040510     c                             or %subst(Kpjbu:1:1) = 'S'
110300000418     C                   movel     kpjbu         TIBS20DS
110400000418     c                   eval      Wi20vid = I20vid
110500040511     c                   Eval      v1ccar = I20Car
110600000418     C                   eval      V1Caas = I20aas
110700000418     C                   eval      V1Clnp = I20lnp
110800000418     C                   eval      V1Cnrs = I20nrs
110900000418     C                   eval      V1Cnsp = I20nsp
111000000418     C                   eval      V1Clna = I20lna
111100000418     C                   eval      *in08 = *on
111200020904     c                   Eval      V1fmd = 'M'
111300040510     c                   Eval      v1cinv = 'SI'
111400040510     c                   Eval      v1ctar = 'SI'
111500040511     c                   Eval      wforza = *Off
111600000418      *  Se emetto il formato pieno non ricevo DS
111700000418     C                   OTHER
111800000418     c                   clear                   Wi20vid
111900000418     C                   ENDSL
112000000317      *
112100140508      * -?Reperimento dati job?
112200140508     c                   exsr      sr_DatiJob
112300000317      *
112400000418      * Imposto P.O. gestione e ne carico la £6
112500000418     C                   IF        SIMFEL = *zeros
112600000317     C                   z-add     46            V1CFGS
112700000317     C                   eval      L6(1) = 046
112800000418      * Non visualizzo "P.O. IN GESTIONE"
112900000418     C                   eval      *in06 = *on
113000000418     C                   eval      *in07 = *on
113100020131     C                   eval      *in04 = *on
113200000418      *
113300000317     C                   ELSE
113400000418      *
113500020523     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
113600140508     C                   IF        DUTlpo = '2' or DUTlpo = *blanks
113700020523     C*                  movel     REMFIL        V1CFGS
113800140508     C                   movel     DUTpou        V1CFGS
113900000418     C                   eval      *in06 = *on
114000000418   X2C                   ELSE
114100000418     C                   movel     SIMFEL        V1CFGS
114200000418    2C                   ENDIF
114300000418      *
114400000317     C                   z-add     v1cfgs        Wfgs
114500000317     C                   clear                   L6
114600000317     C                   exsr      CARL6
114700000317     C                   movea     LIN           L6
114800000317     C                   ENDIF
114900000317
115000000317      * reperisco data e ora
115100000317     C                   TIME                    W0140
115200000320     C                   MOVEL     W0140         WORA
115300000317      * UDATE IN GGMMAAAA
115400000317     C                   MOVE      W0140         WDTGIO
115500000317      * UDATE IN AAAAMMGG
115600000320     C     *eur          MOVEL     WDTGIO        Wdata
115700000320     C     *iso          MOVEL     Wdata         DATEU
115800020507
115900020507      * oggi - 1 anno
116000020507     c                   eval      wdataold = wdata
116100020508     c                   subdur    1:*y          wdataold
116200020507     c     *iso          movel     wdataold      wolddata
116300000317
116400000320      * Apertura files se sede eseguo ovrdspf
116500000317     c                   IF        SIMFEL = *zeros
116600020201     c                   open      Tabel00f
116700020201     c                   open      TITAS30C
116800000417     c                   open      TNCSB03L
116900000418     c                   open      FNLBL01L
117000000418     c                   open      FNLBL02L
117100020903     c                   Open      Titaa30c
117200000317     c                   ELSE
117300000317     c                   open      FNBLP01L
117400000317     c                   open      FNARB01L
117500000317     c                   ENDIF
117600000317      *
117700000317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1  se non sono in sede
117800000317     C                   if        SIMFEL <> 0
117900000317     C                   CLEAR                   TRUL06DS
118000000317     C                   MOVE      '£1'          D06COD
118100000317     C                   MOVEL     SIMFEL        D06KEY
118200000317     C                   MOVEL(P)  TRUL06DS      KPJBU
118300000317     C                   CALL      'TRUL06R'
118400000317     C                   PARM                    KPJBA
118500000317     C                   MOVEL     KPJBU         TRUL06DS
118600000317     C                   MOVEA     LIN           L1
118700000317    2C                   endif
118800000317      *
118900000317     C     kspe          KLIST
119000000317     C                   KFLD                    v1cAAS
119100000317     C                   KFLD                    v1cLNP
119200000317     C                   KFLD                    v1cNRS
119300000317     C                   KFLD                    v1cNSP
119400020903
119500020903     c     Kspea         Klist
119600020903     c                   Kfld                    V1caas
119700020903     c                   Kfld                    V1clnp
119800020903     c                   Kfld                    V1cnrs
119900020903     c                   Kfld                    V1cnsp
120000020903     c                   Kfld                    Wtrc
120100020903
120200020201     C     ktab          KLIST
120300020201     C                   KFLD                    tblkut
120400020201     C                   KFLD                    tblcod
120500020201     C                   KFLD                    tblkey
120600020201     c                   move      1             tblkut
120700020703     c**!!!              move      '3M'          tblcod
120800000317      *
120900121016     C                   EVAL      AutProj = GetProfiloUtenteProj(knmus)
121000000317     C                   ENDSR
121100140508      /free
121200140508       //--------------------------------------------------------------
121300140508       //?Reperimento Dati del job (Utente/Operativi).                 ?
121400140508       //--------------------------------------------------------------
121500140508       BEGSR  sr_DatiJob;
121600140508
121700140508         in(e) §AzUte;
121800140508         if NOT %error;
121900140508           in(e) §DatiUte;
122000140508         endif;
122100140508         if %error or RSut = *blank;
122200140508           tibs34r ( tibs34ds );
122300140508           in §AzUte;
122400140508           in §DatiUte;
122500140508         endif;
122600140508
122700140508       ENDSR;
122800140508      /end-free
122900121016     P*--------------------------------------------------
123000121016     P* Procedure name: GetProfiloUtenteProj
123100121016     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
123200121016     P* Returns:        Profilo utente in Proj.
123300121016     P* Parameter:      priProfiloUtente => Profilo Utente
123400121016     P*--------------------------------------------------
123500121016     P GetProfiloUtenteProj...
123600121016     P                 B
123700121016     D GetProfiloUtenteProj...
123800121016     D                 PI             1A
123900121016     D  priProfiloUtente...
124000121016     D                               10A
124100121016
124200121016     D AutProj         S              1A   STATIC
124300121016
124400121016      /FREE
124500140508
124600140508       exec sql  set option  dynusrprf = *owner,
124700140508                             closqlcsr = *endmod;
124800121016
124900121016       CLEAR AutProj;
125000121016
125100121016       EXEC SQL
125200121016         SELECT CASE
125300121016                WHEN utnAnn = '1' THEN 'A'
125400121016                WHEN utnGrp = 999 THEN 'D'
125500121016                ELSE 'S'
125600121016                END
125700121016           INTO :AutProj
125800121016           FROM anutn00f
125900121016           WHERE utnPrf = :priProfiloUtente
126000121016       ;
126100121016
126200121016       SELECT;
126300121016         WHEN sqlCode < *ZERO;
126400121016           DUMP(A);
126500121016           Autproj = '?';
126600121016       ENDSL;
126700121016
126800121016       RETURN autproj;
126900121016
127000121016      /END-FREE
127100121016     P GetProfiloUtenteProj...
127200121016     P                 E
127300000317      *---------------------------------------------------------------------------------------------
127400121018**         CMD1
127500121019ADDPFM FILE(FICSO00R)                MBR(M000000)
127600121018**         CMD2
127700121019OVRDBF FILE(FICSO00R)                MBR(M000000)
127800121018**         CMD3
127900121019CHKOBJ  OBJ(FICSO00R) OBJTYPE(*FILE) MBR(M000000)
128000000317** MSG  Lungh. 78                                                            *
128100000418Immettere la linea                                                            1
128200000418Linea inesistente o annullata                                                 2
128300000418Richiesta di rigenerazione spedizione eseguita                                3
128400000418Inserire un numero spedizione valido                                          4
128500000418Spedizione inesistente, impossibile rigenerare !                              5
128600000418Per la spedizione esiste il C/Assegno, impossibile rigenerare !               6
128700000419Spedizione precedente il settembre 1999, impossibile rigenerare !             7
128800000418La spedizione esiste, impossibile rigenerare !                                8
128900000418Spedizione in gestione ad altro p.o.                                          9
129000000418Causale richiesta non valida !                                                10
129100000418Impossibile rigenerare bolle legate per la gestione C/Assegni omessi !        11
129200000419Bolla legata: per i Danni richiedere l'ultima figlia o la mamma originale !   12
129300000419Bolla legata con mamma originale precedente il 9/1999, impossibile rigenerare 13
129400020221Si possono rigenerare bolle con causale c/a omessi solo se si è in sede.
129500020201Utente non abilitato alla rigenerazione bolla con causale contrassegni omessi.
129600020221Non è ammessa la rigenerazione di una bolla POSTA con causale c/a omessi.
129700020221Non è ammessa la rigenerazione di una bolla AUTOMATICA con causale c/a omessi.
129800020221Può rigenerare la bolla solo l'utente della fililiale di partenza.
129900020304Bolla legata: per Bolla Firmata richiedere l'ultima figlia o mamma originale! 19
130000020304Immettere il p.o. che effettua la richiesta                                   20
130100020304P.o. immesso errato                                                           21
130200020305Si possono rigenerare bolle con causale 'F' solo se si è in sede              22
130300140508Funzione non ancora attiva                                                    23 LIBERO
130400020305Non è ammessa la rig. di una bolla NON CONSEGNATA con causale Bolla firmata!  24
130500020507Richiesta spedizione più vecchia del limite per l'archiviazione               25
130600020703Non è ammessa la rigenerazione di una bolla di RECUPERO                       26
130700020903Non ammessa la rig. di una bolla con mitt. e dest. non codificati!            27
130800020903Attenzione il mittente non è codificato non si può addebitare                 28
130900020903Attenzione il destinatario non è codificato non si può addebitare             29
131000020903Immettere il falg a chi addebitare                                            30
131100040511Utilizzare F19=Immagine per richiesta bolla firmata.                          31
131200040511Il p.o. non è abilitato alla scannerizzazione.                                32
131300040511Bolla già rigenerata in data xx/xx/xxxx. Enter x Forzare!                     33
