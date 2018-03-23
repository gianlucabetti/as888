000100000316     H DECEDIT('0,') DATEDIT(*DMY/)
000101121016     h dftactgrp(*no)
000300000418      *----------------------------------------------------
000400000316      * Rigenerazione bolle per P.O. da TITAS
000500000418      *----------------------------------------------------
000600000320      *    RICHIESTA RIGENERAZIONE SPEDIZIONE
000700000316      *----------------------------------------------------
000800020903      * A T T E N Z I O N E:
000900020903      *---------------------
001000090423      * VALORI DI RIGCAU
001100020903      * D = DANNI
001200020903      * C = C/A OMESSI
001300020903      * F = BOLLE FIRMATE
001400040510      * S = BOLLE FIRMATE DA SCANNER
001500000418
001600000418      ******************************************************
001700000418      * INDICATORI RISERVATI
001800000418      ******************************************************
001900000418      * 05 - sproteggo il P.O. gestione
002000000418      * 06 - non abilito il tasto funzionale F18
002100000418      * 07 - non emetto il P.O. gestione
002200000418      * 08 - Proteggo numero spedizione e lna se ricevuti dal chiamante
002300020304      * 09 - Richiesta Bolla Firmata
002400000418      * 28 - errore generico DSPF
002500000418      * 30 - comodo
002600000418      * 40 - errore P.O. gestione
002700000418      * 41 - errore spedizione
002800000418      * 42 - errore linea arrivo
002900000418      * 43 - errore causale richiesta
003000020304      * 44 - errore p.o. che effettua la richiesta
003100020903      * 45 - errore Flag addebito M/D
003200000418      * 90 - riemissione formato
003300000418      ******************************************************
003400000418
003500000320     fTIBS20D   CF   E             WORKSTN
003600090417     FFnRIG00F  O  A E             DISK
003700030321     FFNBLP01L  IF   E           K DISK    usropn
003800000317     FFNARB01L  IF   E           K DISK    usropn
003900000317     FTITAS30C  IF   E           K DISK    usropn
004000030321     FTNCSB03L  uF   E           K DISK    usropn
004100000418     FFNLBL01L  IF   E           K DISK    usropn  rename(fnlbl000:fnlbl001)
004200000418     FFNLBL02L  IF   E           K DISK    usropn  rename(fnlbl000:fnlbl002)
004300020201     Ftabel00f  IF   E           K DISK    usropn
004400000320     FAZORG01L  IF   E           K DISK
004500020903     fTitaa30c  if   e           k Disk    usropn
004600090417     fFnrig02l  if   e           k Disk            rename(fnrig000:Fnrig02)
004601121019     fficso00r  o    e           k disk            usropn
004700000316      *----------------------------------------------------
004800000317      *   S C H I E R E
004801121018     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
004802121018     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
004803121018     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
004900040510     D MSG             S             78    DIM(35) CTDATA PERRCD(1)
005000000418     D L1              S              3  0 DIM(30)
005100000418     D L6              S              3  0 DIM(30)
005200000317      *
005300000317      *   D S   I N T E R N E / E S T E R N E
005400000317      *
005500020204     D og143         E DS
005600020204     D TIBS20DS      E DS
005700000320     D FNLV50DS      E DS
005800000317     D UT§DSE0F      E DS
005900000317     D CNCR80        E DS
006000000418     D DSCMZ         E DS
006001121022     D Fidna4ds      E DS
006002121025     D Fidna3ds      E DS
006100000417     D KPJBA         E DS
006101121018     D  LIBSYS                92    101
006102121018     D  LEGFIL               483    492
006103121018     D  MBRFIL               493    502
006200000317     D TRUL06DS      E DS
006300000418     D  LIN                    1     90  0    DIM(30)
006400020703
006500020703     d ds3a          e ds
006600020806     d Tibs55Ds      e ds
006700020903     d tibs69ds      e ds
006800020903     d ds_cnaco      e ds                  extname(cnaco00f)
006900020903     d ds_cnind      e ds                  extname(cnind00f)
007000020903     d ds_cnclp      e ds                  extname(cnclp00f)
007100020903     d ds_fncls      e ds                  extname(fncls00f)
007200040510     d dtasflo       e ds
007300040510     d og146         e ds
007400040511
007500040511     d dtaimm          ds                  inz
007600040511     d  anno                   1      4
007700040511     d  mese                   5      6
007800040511     d  giorno                 7      8
007900020903
007901121019     d §membro         ds
007902121018     d  §emme                  1      1    inz('M')
007903121018     d  §fle                   2      4s 0
007904121018     d  §sed                   5      7s 0 inz(046)
007905121018     d  §mbr                   2      7
007911121022     d
008000000317      *
008100000317     D                SDS
008200000317     D  VTCPGM                 1     10
008300000320      *
008400000320      *   C A M P I
008500000320      *
008600000320     D W0140           S             14  0 inz
008700020806     D W236            S            236
008800000320     D cmzlcl          S              1
008900000418     D wlegame         S              1
009000000418     D Wrec            S              7  0
009100000320     D Wdtgio          S              8  0
009200000320     D dateu           S              8  0
009300000320     D Wora            S              6
009301121025     D skpjbu          S                   LIKE(kpjbu)
009400000321     D wfgs            S                   LIKE(V1Cfgs)
009500000418     D Wi20vid         S                   LIKE(I20vid)
009600020213     D Wdata           S               D   DATFMT(*eur)
009700020507     d wdataold        S               d   datfmt(*eur)
009800020507     d wolddata        s              8  0
009900020507     d wtasdta         s              8  0
010000020213     D OUTQA           S             10
010100020213     D OUTQP           S             10
010200020213     D COM03           S              3
010300020304     d flagpo          s              1
010400020903     d Wtrc            s              1
010500020903     d Wtb1            s              1
010600020903     d Wccm            s              4  0
010700020903     d Wccd            s              4  0
010800021014      *parametri per reperimento coda
010900021014     d finep           s              1
011000021014     d codpo           s              3s 0
011100021014     d tpcoda          s              1
011200021014     d libcoda         s             10
011300021014     d esito           s              1
011400021014     D OUTQ            S             10
011500040511
011600040511     d wforza          s              1    inz(*Off)
011700040511     d datamsg         s             10
011800040511     d w1ccar          s                   like(v1ccar)
011900120306     d savsif          s                   like(knsif)
011901121016     d Autproj         s              1
011902121016     D*--------------------------------------------------
011903121016     D* Procedure name: GetProfiloUtenteProj
011904121016     D* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
011905121016     D* Returns:        Profilo utente in Proj.
011906121016     D* Parameter:      priProfiloUtente => Profilo Utente
011907121016     D*--------------------------------------------------
011908121016     D GetProfiloUtenteProj...
011909121016     D                 PR             1A
011910121016     D  priProfiloUtente...
011911121016     D                               10A
011912121016
012000000317      *------------------------------------------------------------------------*
012100000317      *---------------  C A L C O L O  ----------------------------------------*
012200000317      *------------------------------------------------------------------------*
012300000317
012400000320      * Emetto DSPF x richiesta parametri se non li ho ricevuti dal chiamante
012500000418     c                   IF        WI20vid <> 'N'
012600000317
012700000317      * Emissione VIDEO
012800000320     C     for01         tag
012900000317     C                   exsr      EMISd01
013000000317
013100000317      * f3=Fine
013200000317     c   kc              goto      fine
013300000317
013400000317      * f18=Cambio P.O. in gestione
013500000317     c   ks              eval      *in05 = *on
013600000317     c   ks              goto      for01
013700000317
013800000317      * 05 on --> controllo P.O. in gestione
013900000317     c     *in05         ifeq      *on
014000000317     C                   exsr      ctrfgs
014100000317     c   90              goto      for01
014200000317     c                   eval      *in05 = *off
014300000317     c                   end
014400000317
014500000317      * Controlli videata
014600000317     c                   exsr      ctrd01
014700000317
014800000317     C                   IF        *IN90 or *IN28 or *INKF = *off
014900020304     c                             or (*in09 and flagpo <> 'S')
015000000317     c                   goto      for01
015100000317     c                   ENDIF
015200090423
015300090417      * Imposto dati nella DS per scrivere FnRIG
015400000418     C                   IF        *INKF = *on
015500030321      *
015600030321      * Se rigenera per i DANNI e c'è un C/A già incassato, deve riattivare
015700030321      * la trasmissione del TNCSB sfleggando il flag e la data di trasmissione.
015800030321      *  e se di sede
015900030321     c                   if        V1Ccar = 'D' and
016000030321     c                             SIMFEL = *zeros
016100030321     c                   exsr      UPD_TNCSB
016200030321     c                   endIF
016300030321      *
016400000418     C                   clear                   TIBS20DS
016500000418     C                   eval      I20aas = V1Caas
016600000418     C                   eval      I20lnp = V1Clnp
016700000418     C                   eval      I20nrs = V1Cnrs
016800000418     C                   eval      I20nsp = V1Cnsp
016900000418     C                   eval      I20lna = V1Clna
017000000418     C                   eval      I20car = V1Ccar
017100000418     C                   eval      I20tra = 'E'
017200000418     C                   eval      I20por = V1Cfgs
017300020304
017400090423     c                   if        v1ccar = 'F' or v1ccar = 'S'
017500090423     c                   eval      i20por = v1cpor
017600090423     c                   eval      i20tra = 'N'
017700090423     c                   endif
017800020304
017900000418     C                   eval      I20pru = KNMUS
018000000418     C                   eval      I20dim = DATEU
018100000418     C                   movel     wora          I20him
018200000418     c                   ENDIF
018300090423
018400020131     c* per contrassegni omessi chiamo il pgm TNSC03R per immettere il
018500020131     c* contrassegno
018600121011     c                   if        v1ccar = '3' or v1ccar = '8'
018700020131     c                   movel(p)  tibs20ds      kpjbu
018800020131     c                   call      'TNSC03R'
018900020131     c                   parm                    kpjba
019000020131     c                   movel     kpjbu         tibs20ds
019100020131     c* se non convalido il contrassegno
019200020301     c                   if        o20err = '1'
019300020301     C                   eval      V1cMSG = o20msg
019400020301     c                   if        o20msg <> *blanks
019500020301     c                   seton                                        28
019600020301     c                   ENDIF
019700020131     c                   goto      for01
019800020131     c                   ENDIF
019900020131     c* chiamo pgm di stampa lettera per mittente/destinatario
019901121018     c                   if        v1ccar = '3'
020000020207     c                   call      'TNSC04C'
020100020131     c                   parm                    kpjba
020200020213     c                   parm                    OUTQA
020300020213     c                   parm                    OUTQP
020301121018     c                   endif
020400020131     c                   ENDIF
020500020131
020600000320     c                   ENDIF
020700000320
020800000418      * Scrivo file
020900000320     C                   exsr      Wrtfirig
020901121025     c                   movel     kpjbu         skpjbu
021000121025      * richiamo programmi per inserimento note
021001121025     c                   if        RigCau = '8' or RigCau = '3'
021002121025     c                   clear                   fidna4ds
021003121025     c                   if        RigCau = '8'
021004121025     c                   move      '1'           IA4MOD
021005121025     c                   else
021006121025     c                   move      '2'           IA4MOD
021007121025     c                   endif
021008121025      *
021009121025     c                   move      ' 30'         IA4MAD
021010121025     c                   move      'S'           IA4TOR
021011121025     c                   eval      IA4OGG = %editc(i20lnp: 'X') +
021012121025     c                             %editc(i20nrs: 'X') +
021013121025     c                             %editc(i20nsp: 'X') + %editc(i20aas: 'X')
021014121025     c                   eval      IA4NO2 = 'BOLLA RIGENERATA PER C/ASSEGNO -
021015121025     c                             OMESSO IL ' + (%editc(wdtgio:'Y'))
021016121025     c                   move      'TIBS20R'     IA4PGM
021017121025      *linea estero
021018121025     c                   if        §ogntw = 'DPD' or
021019121025     c                             §ogntw = 'FED' or
021020121025     c                             §ogntw = 'EEX'
021021121025     c                   eval      IA3MAD = IA4MAD
021022121025     c                   eval      IA3TOR = IA4TOR
021023121025     c                   eval      IA3OGG = IA4OGG
021024121025     c                   clear                   IA3NO1
021025121025     c                   eval      IA3NO2 = IA4NO2
021026121025     c                   clear                   IA3ARA
021028121025     c                   call      'FIDNA3R'
021029121025     c                   parm                    kpjba
021030121025     c                   parm                    fidna3ds
021031121025      * linea italia
021032121025     c                   else
021033121025     c                   movel     fidna4ds      kpjbu
021034121025     c                   call      'FIDNA4R'
021035121025     c                   parm                    kpjba
021036121025     c                   endif
021037121025     c                   endif
021038121025     c                   movel     skpjbu        kpjbu
021100000418      * Riemetto il formato
021200000420     c                   IF        WI20vid <> 'N'
021300000320     C                   clear                   V1Caas
021400000320     C                   clear                   V1Clnp
021500000320     C                   clear                   V1Cnrs
021600000320     C                   clear                   V1Cnsp
021700000320     C                   clear                   V1Clna
021800000320     C                   clear                   V1Dlna
021900000320     C                   clear                   V1Ccar
022000020304     c                   clear                   v1cpor
022100020304     c                   clear                   v1dpor
022200020903     c                   Clear                   V1cccm
022300020903     c                   Clear                   V1cccd
022400020903     c                   Clear                   V1dccm
022500020903     c                   Clear                   V1dccd
022600020904     c                   Movel     'M'           V1fmd
022700040510     c                   Eval      v1cinv = 'SI'
022800040510     c                   Eval      v1ctar = 'SI'
022900040511     c                   Eval      wforza = *Off
023000000320     C                   eval      *in28 = *on
023100000320     c                   eval      v1cmsg = msg(3)
023200000320     c                   goto      for01
023300000420     c                   ENDIF
023400000317
023500000317     c     fine          tag
023600120306     c                   eval      savsif=knsif
023700000317
023800000418      * Elaboro record scritti
023900000320     C                   SELECT
024000000321     C                   WHEN      wrec = *zeros
024100000418     C                   WHEN      SIMFEL = *zeros
024200120306     c                   if        %subst(knsif:7:1)='P'
024300120306     c                   eval      knsif='GAITRAPSP '
024400120306     c                   else
024500120306     c                   eval      knsif='GAITRA201 '
024600120306     c                   endif
024700120306
024800040510     c                   If        wi20vid <> 'S'
024801121022     c                   if        RigCau = '8'
024802121018      *forza scrittura su file spia e richiama programma di ricezione del rekord
024803121018      *in modo forzato
024804121018     c                   exsr      ficso
024805121018     c                   movel     §membro       mbrfil
024806121018     c                   eval      kcoaz = 'LRO2'
024807121018     c                   call      'BCH10'
024808121018     c                   parm                    kpjba
024809121018     c                   else
024900000418     C                   clear                   KPJBU
025000000418     c                   eval      kcoaz = 'BS21'
025100020827     c                   call      'BCH10'
025200000418     c                   parm                    kpjba
025201121018     c                   endif
025300040510     c                   Else
025400040510     C                   clear                   KPJBU
025500040510     c                   eval      kcoaz = 'BS25'
025600040513     c                   call      'BCH10'
025700040510     c                   parm                    kpjba
025800040510     c                   EndIf
025900121022     c                   ENDSL
026000121022     c                   eval      knsif=savsif
026100121022
026200000317     c                   EVAL      *INLR = *ON
026300030321      **********************************************************************
026400030321      * Solo per i DANNI aggiorna
026500030321      **********************************************************************
026600030321     C     UPD_TNCSB     BegSR
026700030321      *
026800030321      * se Trovato un C/A e già incassato deve sflaggare per essere
026900030321      *  ritrasmesso.
027000030321     C     kspe          chain     tncsb03L
027100030321     c                   if        %found(tncsb03l)
027200030321      *  se già incassato:data arrivo distinta incasso <> 0
027300030321     c                   if        csbdta >0
027400030321     c                   move      *blank        csbftr
027500030321     c                   move      *zeros        csbdtr
027600030321     c                   end
027700030321     C                   update    tncsb000
027800030321     c                   end
027900030321      *
028000030321     C                   EndSR
028100000317      **********************************************************************
028200000317      * EMISSIONE VIDEO 1
028300000317      **********************************************************************
028400000317     C     EMISD01       BEGSR
028500000317
028600040510     c                   Select
028700040510     c                   When      WI20vid = 'W'
028800000417     c                   exfmt     BS20W01
028900040510     c                   When      WI20vid = 'S'
029000040510     c                   exfmt     BS20W02
029100040510     C                   Other
029200000317     c                   write     BS20T01
029300000317     c                   write     BS20Z01
029400000417     c                   exfmt     BS20D01
029500040510     c                   ENDsl
029600000317
029700000317      * Azzero Campi ed Indicatori
029800000320     c                   eval      *IN41 = *OFF
029900000320     c                   eval      *IN42 = *OFF
030000000418     c                   eval      *IN43 = *OFF
030100020304     c                   eval      *in44 = *off
030200020903     c                   Eval      *In45 = *off
030300000317     c                   eval      *IN28 = *off
030400000317     c                   eval      *IN90 = *off
030500000317     c                   clear                   v1cmsg
030600020304
030700020903      * se ho riemesso la videata per *in09 imposto flag
030800020304     c                   if        *in09
030900020304     c                   eval      flagpo = 'S'
031000020304     c                   else
031100020304     c                   clear                   flagpo
031200020304     c                   endif
031300020304
031400000317
031500000317     C                   ENDSR
031600000317      **********************************************************************
031700000317      * CONTROLLO IL P.O. IN IN GESTIONE
031800000317      **********************************************************************
031900000317     C     CTRFGS        beGSR
032000000317      *
032100000320     c                   eval      *in40 = *off
032200000317     c                   eval      *in90 = *off
032300000317     c                   clear                   v1dfgs
032400000317     C                   clear                   FNLV50DS
032500000317     C                   movel     KNMUS         D50PRU
032600000317     C                   movel     V1cfgs        D50FGS
032700000317      *
032800000317     C                   CALL      'FNLV50R'
032900000317     C                   PARM                    FNLV50DS
033000000317      *
033100000317     C                   IF        D50ERR <> *blanks
033200000317     C                   movel     D50MSG        V1cMSG
033300000317     c                   eval      *in28 = *on
033400000317     c                   eval      *in40 = *on
033500000317     c                   eval      *in90 = *on
033600000317     C                   else
033700000317      *
033800000317     C                   z-add     v1cfgs        Wfgs
033900000317     C                   clear                   L6
034000000317     C                   EXSR      CARL6
034100000317     C                   movea     LIN           L6
034200000317      *
034300000317      * decodifica
034400000317     c     v1cfgs        CHAIN     azorg
034500000317     c                   IF        %FOUND
034600000317     C                   movel     orgDES        V1DFGS
034700000317    2C                   ENDIF
034800000317      *
034900000317     C                   ENDIF
035000000317      *
035100000317     c                   ENDSR
035200000317      **********************************************************************
035300000317      * CONTROLLI VIDEO 1
035400000317      **********************************************************************
035500000317     C     Ctrd01        BEGSR
035600020305
035700000317      *
035800000317      * Controllo linea di partenza
035900000317     C                   IF        V1Clnp = *zeros
036000000320     c                   MOVEL     MSG(1)        v1CMSG
036100000317     c                   eval      *in28 = *on
036200000317     c                   ELSE
036300000317      *
036400000317     c     V1Clnp        chain     azorg01l
036500000317     c                   If        not %Found  OR  orgfva <> *blanks
036600000320     c                   movel     MSG(2)        v1CMSG
036700000317     c                   eval      *in28 = *on
036800000317     c                   Endif
036900000317     c                   ENDIF
037000000317      *
037100000317     c                   IF        *IN28 = *ON
037200000320     c                   eval        *IN41 = *on
037300000317     c                   goto      ev1spe
037400000317     C                   ENDIF
037500021014      * reperisce outq della linea di partenza
037600021014     c                   z-add     v1clnp        codpo
037700021014     c                   exsr      sroutq
037800021014     c                   movel     outq          outqp
037900000317      *   sistemo anno di due cifre
038000000317     C                   If        V1Caas < 100  and V1Caas > 60
038100000317     C                   add       1900          V1Caas
038200000317     C                   Endif
038300000317     C                   If        V1Caas < 100  and V1Caas <= 60
038400000317     C                   add       2000          V1Caas
038500000317     C                   Endif
038600000317      *  Controllo key spedizione
038700020221     c                   IF        V1clnp = *zeros  or  V1Cnsp = *zeros
038800020221     c                   eval        *IN28 = *on
038900000320     c                   movel     MSG(4)        v1CMSG
039000000317     c                   goto      ev1spe
039100000317     C                   ENDIF
039200000317      *
039300000317      * Controllo linea di arrivo
039400000317     C                   IF        V1Clna = *zeros
039500000320     c                   movel     MSG(1)        v1CMSG
039600000317     c                   eval      *in28 = *on
039700000317     c                   ELSE
039800000317      *
039900000317     c     V1Clna        chain     azorg01l
040000000317     c                   If        not %Found  OR  orgfva <> *blanks
040100000320     c                   movel     MSG(2)        v1CMSG
040200000317     c                   eval      *in28 = *on
040300000317     c                   Endif
040400040331     c* se linea arrivo DPD e rigeneraz. x contrassegni omessi errore
040500040331     c                   If        %Found and  orgfva = *blanks
040600121011     c                             and (V1Ccar = '3' or v1ccar = '8')
040700040331     c                   MOVEL     ORGDE3        OG143
040701121023     c                   select
040800121023     c                   when      §ogntw = 'DPD' or
040801121023     c                             §ogntw = 'FED'
040900040331     c                   movel     MSG(21)       v1CMSG
041000040331     c                   eval      *in28 = *on
041001121026     c                   when      §ogntw = 'EEX' and v1ccar = '8'
041002121029     c                   movel     MSG(10)       v1CMSG
041003121023     c                   eval      *in28 = *on
041100121023     c                   Endsl
041200040331     c                   Endif
041300000317     c                   ENDIF
041400000317      *
041500000317     c                   IF        *IN28 = *ON
041600000320     c                   eval        *IN42 = *on
041700000317     c                   goto      ev1spe
041800000317     C                   ENDIF
041900021014     c* reperisce outq della linea di arrivo
042000021014     c                   z-add     v1clna        codpo
042100021014     c                   exsr      sroutq
042200021014     c                   movel     outq          outqa
042300000418      * Controllo causale richiesta
042400121011     c                   IF        V1Ccar <> 'D'
042500040510     c                             and v1ccar <> 'F' and v1ccar <> 'S'
042501121011     c                             and v1ccar <> '3' and v1ccar <> '8'
042600000418     c                   movel     MSG(10)       v1CMSG
042700000418     c                   eval      *in28 = *on
042800000418     c                   eval      *IN43 = *on
042900000418     c                   goto      ev1spe
043000000418     C                   ENDIF
043100040510
043200040510s   1c                   Select
043300020204      * la causale contrassegni omessi ('C') è abilitata solo:
043400121019      * se si è in sede    le causali sono diventate 3 e 8 non più C
043500121011w   1c                   when      V1Ccar = '3' or v1ccar = '8'
043600020201     c                   if        simfel <> *zeros
043700020201     c                   movel     MSG(14)       v1CMSG
043800020201     c                   eval      *in28 = *on
043900020201     c                   eval      *IN43 = *on
044000020201     c                   goto      ev1spe
044100020201     C                   ENDIF
044200020212      * se l'utente è 'FILxxx' oppure 'CON*' oppure 'EDP*'
044201121016     c                   if        Autproj <> 'S'
044300121016     c*m                 if        %subst(knmus: 1: 3) <> 'FIL' and
044400121016     c*m                           %subst(knmus: 1: 3) <> 'CON' and
044500121016     c*m                           %subst(knmus: 1: 3) <> 'EDP'
044600020201     c                   movel     MSG(15)       v1CMSG
044700020201     c                   eval      *in28 = *on
044800020201     c                   eval      *IN43 = *on
044900020201     c                   goto      ev1spe
045000020204     c                   end
045100020206     c                   move      v1clnp        com03             3
045101121016     c                   if        simpou <> 046
045102121016     c                   if        v1clnp <> simpou
045103121016     c*m                 if        %subst(knmus: 1: 3) =  'FIL' and
045104121016     c*m                           %subst(knmus: 4: 3) <> com03
045400020204     c                   movel     MSG(18)       v1CMSG
045500020204     c                   eval      *in28 = *on
045600020204     c                   eval      *IN43 = *on
045700020204     c                   goto      ev1spe
045800020201     C                   ENDIF
045801121016     C                   Endif
045900040510
046000020304      * la causale bolla firmata ('F') è abilitata solo:
046100020304      * se si è in sede
046200040510w   1c                   When      V1Ccar = 'F'
046300040510     c                   if        simfel <> *zeros
046400020304     c                   movel     MSG(22)       v1CMSG
046500020304     c                   eval      *in28 = *on
046600020304     c                   eval      *IN43 = *on
046700020304     c                   goto      ev1spe
046800020304     C                   endif
046900040510
047000040511      * Se richiesta bolla firmata scanner ('S')
047100040510w   1c                   When      v1ccar = 'S'
047200040510      * - devo riemettere la videata se richiedono l'invio della bolla rigenerata al cliente
047300040510if  2c                   If        flagpo <> 'S'
047400040510     c                   If        v1cinv = 'SI'
047500040510     c                   Eval      *In09 = *On
047600040510     c                   EndIf
047700040510      * - se no esco dal pgm
047800040510     c                   If        v1cinv = 'NO'
047900040510     c                   Goto      fine
048000040510     c                   EndIf
048100040510e   2c                   EndIf
048200040510e   1c                   EndSl
048300040510
048400000418      * Se sono in SEDE controllo che:
048500000418     C                   IF        SIMFEL = *zeros
048600000317     C     kspe          chain     titas30c
048700000418      *
048800000418     c                   SELECT
048900000418      *  la spedizione deve esistere
049000000418     c                   WHEN      not %found(titas30c)
049100000418     c                   movel     MSG(5)        v1CMSG
049200000317     c                   eval      *in28 = *on
049300000418      *  la spedizione non deve essere antecedente al settembre 1999
049400000418     c                   WHEN      TASaas < 1999  or
049500000418     c                             (TASaas = 1999  and  TASmgs < 0901)
049600000418     c                   movel     MSG(7)        v1CMSG
049700000418     c                   eval      *in28 = *on
049800040510
049900020204      * se la causale è "C/A omesso"
050000121011     C                   WHEN      V1Ccar = '3' or v1ccar = '8'
050100020201      * la bolla non deve essere POSTA
050200020225     c                   if        §ogntw = 'PPT'
050300020201     c                   movel     MSG(16)       v1CMSG
050400020201     c                   eval      *in28 = *on
050500020201     c                   eval      *IN43 = *on
050600020201     c                   goto      ev1spe
050700020201     C                   ENDIF
050800020204      * la bolla non deve essere AUTOMATICA solo per utenti FIL*
050801121016     c                   if        simpou <> 046
050900121016     c*m                 if        %subst(knmus: 1: 3) =  'FIL'
051000020703     c                   move      '3M'          tblcod
051100020201     c                   movel(p)  tassop        tblkey
051200020201     c     ktab          setll     tabel00f                               28
051300020201     c                   if        *in28
051400020201     c                   movel     MSG(17)       v1CMSG
051500020201     c                   eval      *IN43 = *on
051600020201     c                   goto      ev1spe
051700020201     C                   ENDIF
051800020204     C                   ENDIF
051900020201      *  il C/A non deve esistere                                      annullato
052000000417     C     kspe          setll     tncsb000
052100000417     c                   If        %equal
052200000418     c                   movel     MSG(6)        v1CMSG
052300000417     c                   eval      *in28 = *on
052400020201     c                   goto      ev1spe
052500000417     c                   Endif
052600040510
052700020305      * Se causale è Bolla Firmata
052800020305     c                   when      v1ccar = 'F'
052900040511     c                   Eval      dtasflo = tasflo
053000020305      * la spedizione deve essere consegnata
053100041118     c                   if        tasndc = *zeros or TasDcm = *Zeros
053200020305     c                   eval      v1cmsg = msg(24)
053300020305     c                   eval      *in28 = *on
053400020305     c                   goto      ev1spe
053500020305     c                   endif
053600020507      * la spedizione non deve essere più vecchia di un anno
053700020507     c                   movel     tasaas        wtasdta
053800020507     c                   move      tasmgs        wtasdta
053900020507     c                   if        wtasdta < wolddata
054000020507     c                   eval      v1cmsg = msg(25)
054100020507     c                   eval      *in28 = *on
054200020507     c                   goto      ev1spe
054300020507     c                   endif
054400000418     C                   ENDSL
054500020703
054600020703      *  la spedizione non deve essere una bolla di recupero
054700020703     c                   clear                   ds3a
054800020703     c                   move      '3A'          tblcod
054900020703     c                   movel(p)  tascbo        tblkey
055000020703     c     ktab          chain     tabel00f
055100020703     c                   if        %found(tabel00f) and tblflg = *blanks
055200020703     c                   movel     tbluni        ds3a
055300020703     c                   if        §3arbl = 'R'
055400040511     c                             and V1ccar <> 'S'
055500020703     c                   eval      *in28 = *on
055600020703     c                   eval      v1cmsg = msg(26)
055700020703     c                   goto      ev1spe
055800020703     c                   endif
055900020703     c                   endif
056000020903
056100020903      * Se richiesta Bolla Firmata imposto il mittente e il destinatario in base al tipo bolla
056200040510      * anche se bolla firmata da scanner
056300020903 b1  c                   If        V1ccar = 'F'
056400040510     c                             or V1ccar = 'S'
056500020903     c                   Movel     §3atb1        Wtb1
056600020903 b2  c                   If        Wtb1 = 'F'
056700020903     c                   Movel     Tasccm        V1cccm
056800020903     c                   Movel     V1clna        V1cccd
056900020903     c                   Move      9999          V1cccd
057000020903 e2  c                   EndIf
057100020903 b2  c                   If        Wtb1 = 'A'
057200020903     c                   Movel     Tasccm        V1cccm
057300020903     c                   Movel     TasKsc        V1cccd
057400020903 e2  c                   EndIf
057500020903
057600020903     c                   Move      V1cccm        Wccm
057700020903     c                   Move      V1cccd        Wccd
057800020903     c                   Clear                   V1dccm
057900020903     c                   Clear                   V1dccd
058000020903      * Recupero le ragioni sociali
058100020903      * Da titaa se mittente non codificato
058200020903 b2  c                   If        Wccm = 8888 or Wccm = 9999
058300020903     c                   Eval      Wtrc = 'M'
058400020903     c     Kspea         Chain     Titaa30c
058500020903 b3  c                   If        %Found(Titaa30c)
058600020903     c                   Movel     Taarsc        V1dccm
058700020903 e3  c                   EndIf
058800020903 x2  c                   Else
058900020903      * Da Piano dei conti se mittente codificato
059000020903     c                   clear                   TIBS69DS
059100020903     c                   Movel     V1cccm        I69kac
059200020903     c                   call      'TIBS69R'
059300020903     c                   parm                    tibs69DS
059400020903     c                   parm                    ds_cnaco
059500020903     c                   parm                    ds_cnind
059600020903     c                   parm                    ds_cnclp
059700020903     c                   parm                    ds_fncls
059800020903 b3  c                   if        o69err <> '1'
059900020903     c                   movel     acorag        V1dccm
060000020903 e3  c                   EndIf
060100020903 e2  c                   EndIf
060200020903      * Da titas se destinatario non codificato
060300020903 b2  c                   If        Wccd = 8888 or Wccd = 9999
060400020903     c                   Movel     Tasrsd        V1dccd
060500020903 x2  c                   Else
060600020903     c                   clear                   TIBS69DS
060700020903     c                   Movel     V1cccd        I69kac
060800020903     c                   call      'TIBS69R'
060900020903     c                   parm                    tibs69DS
061000020903     c                   parm                    ds_cnaco
061100020903     c                   parm                    ds_cnind
061200020903     c                   parm                    ds_cnclp
061300020903     c                   parm                    ds_fncls
061400020903 b3  c                   if        o69err <> '1'
061500020903     c                   movel     acorag        V1dccd
061600020903 e3  c                   EndIf
061700020903 e2  c                   EndIf
061800020903 e1  c                   EndIf
061900000418      *
062000000418      * Controllo Legami Bolla
062100000418     c                   exsr      legbol
062200000418     C                   SELECT
062300000419      *   Se causale C/A omessi non elaboro le bolle legate
062400121011     C                   WHEN      wlegame <> *blanks and
062401121011     C                             (V1Ccar = '3'  or v1ccar = '8')
062500000418     c                   movel     MSG(11)       v1CMSG
062600000418     c                   eval      *in28 = *on
062700000419      *   Se causale Danni elaboro solo mamma originale o ultima figlia
062800000418     C                   WHEN      V1Ccar = 'D'  and  wlegame = 'X'
062900000418     c                   movel     MSG(12)       v1CMSG
063000000418     c                   eval      *in28 = *on
063100020304      *   Se causale Bolla firmata elaboro solo mamma originale o ultima figlia
063200020304     C                   WHEN      V1Ccar = 'F'  and  wlegame = 'X'
063300020304     c                   movel     MSG(19)       v1CMSG
063400020304     c                   eval      *in28 = *on
063500000419      *   Controllo se la mamma originale rientra nei limiti di data 9/99
063600000419     C                   WHEN      Wlegame = 'S'  and  (LBLaao < 1999  or
063700000419     c                                  (LBLaao = 1999  and  LBLmgo < 0901))
063800000419     c                   movel     MSG(13)       v1CMSG
063900000419     c                   eval      *in28 = *on
064000000418     C                   ENDSL
064100020304
064200020304      * Se richiesta Bolla Firmata devo riemettere la videata per dare la
064300020304      * possibilità di inserire il p.o. che effettua la richiesta.
064400040510     c                   if        v1ccar = 'F' or v1ccar = 'S'
064500020304      * se già riemessa la videata
064600020304     c                   if        flagpo = 'S'
064700020903      * La bolla firmata può essere rigenerata solo se il mittente o il destinatario sono
064800020903      * dei clienti codificati
064900020903     c                   If        (Wccm = 8888 or Wccm = 9999) and
065000020903     c                             (Wccd = 8888 or Wccd = 9999)
065100020903     c                   Movel     Msg(27)       V1cmsg
065200020903     c                   Eval      *In28 = *On
065300020903     c                   Eval      *In41 = *On
065400020903     c                   GoTo      Ev1spe
065500020903     c                   EndIf
065600020903      * Immettere il flag di addebito
065700020903     c                   If        V1fmd = *blanks
065800020903     c                   Movel     Msg(30)       V1cmsg
065900020903     c                   Eval      *In28 = *On
066000020903     c                   Eval      *In45 = *On
066100020903     c                   GoTo      Ev1spe
066200020903     c                   EndIf
066300020903      * Se la bolla va addebitata al mittente  deve essere un codificato
066400020903     c                   If        V1fmd = 'M' and
066500020903     c                             (Wccm = 8888 or Wccm = 9999)
066600020903     c                   Movel     Msg(28)       V1cmsg
066700020903     c                   Eval      *In28 = *On
066800020903     c                   Eval      *In45 = *On
066900020903     c                   GoTo      Ev1spe
067000020903     c                   EndIf
067100020903      * Se la bolla va addebitata al destinatario  deve essere un codificato
067200020903     c                   If        V1fmd = 'D' and
067300020903     c                             (Wccd = 8888 or Wccd = 9999)
067400020903     c                   Movel     Msg(29)       V1cmsg
067500020903     c                   Eval      *In28 = *On
067600020903     c                   Eval      *In45 = *On
067700020903     c                   GoTo      Ev1spe
067800020903     c                   EndIf
067900040510      * controllo che non sia stata già fatta una richiesta di rigenerazione per questa bolla
068000040510      * se bolla firmata scanner
068100040510if  3c                   If        v1ccar = 'S'
068200090417     c     kspe          Setll     Fnrig02l
068300040510     c                   Do        *Hival
068400090417     c     kspe          Reade     Fnrig02l
068500090417     c                   If        %Eof(Fnrig02l)
068600040510     c                   Leave
068700040510     c                   EndIf
068800040511     c                   Clear                   w1ccar
068900091202     c                   if        rigcau = v1ccar
069000040511     c                   Select
069100091202     c                   when      v1ctar = 'SI' and V1fmd = 'M' and
069200091202     c                             rigadd = 'M' and rigtas = 'T'
069300091202     c                   Eval      w1ccar = 'S'
069400091202     c                   when      v1ctar = 'SI' and V1fmd = 'D' and
069500091202     c                             rigadd = 'D' and rigtas = 'T'
069600091202     c                   Eval      w1ccar = 'S'
069700091202     c                   when      v1ctar = 'NO' and V1fmd = 'M' and
069800091202     c                             rigadd = 'M' and rigtas = 'N'
069900091202     c                   Eval      w1ccar = 'S'
070000091202     c                   when      v1ctar = 'NO' and V1fmd = 'D' and
070100091202     c                             rigadd = 'D' and rigtas = 'N'
070200091202     c                   Eval      w1ccar = 'S'
070300040511     c                   EndSl
070400040510      * controllo se dello stesso tipo
070500090417     c                   If        rigcau = w1ccar and wforza = *Off
070600090423     c                   Movel     rigdata       dtaimm
070700040510     c                   Eval      *In28 = *On
070800040511     c                   Eval      v1cmsg = msg(33)
070900040511     c                   Eval      datamsg = %trim (giorno + '/' +
071000040511     c                             mese + '/' + anno)
071100040511     c                   Eval      %subst(v1cmsg:30:10) = datamsg
071200040510     c                   Eval      wforza = *On
071300040510     c                   Goto      ev1spe
071400040510     c                   EndIf
071500091202     c                   endif
071600040511     c                   EndDo
071700040510    3c                   EndIf
071800020903
071900020304      * P.O. obbligatorio
072000020304     c                   if        v1cpor = *zeros
072100020304     c                   movel     msg(20)       v1cmsg
072200020304     c                   eval      *in28 = *on
072300020304     c                   eval      *in44 = *on
072400020304     c                   goto      ev1spe
072500020304     c                   endif
072600020304      * Controllo p.o. immesso
072700020304     c     v1cpor        chain     azorg01l
072800020304     c                   if        not %found(azorg01l) or orgfva <> *blanks or
072900020304     c                             (orgfag <> 'F' and orgfag <> 'A')
073000020304     c                   movel     msg(21)       v1cmsg
073100020304     c                   eval      *in28 = *on
073200020304     c                   eval      *in44 = *on
073300020304     c                   goto      ev1spe
073400020304     c                   endif
073500020304     c                   movel     orgdes        v1dpor
073600040510      * se bolla firmata e il p.o. che effettua la richiesta è abilitato alla scannerizzazione
073700040511      * e esiste l'immagine della LDV errore bloccante
073800040510     c                   Eval      Og146 = orgde6
073900040511     c                   If        §Ogdse > *Zeros and v1ccar = 'F' and
074000040511     c                             §floiml <> *Blanks
074100040510     c                   Eval      v1cmsg = msg(31)
074200040510     c                   Eval      *In28 = *On
074300040510     c                   Eval      *In44 = *On
074400040510     c                   Goto      ev1spe
074500040510     c                   EndIf
074600040510      * se bolla firmata da scanner e il p.o. che effettua la richiesta non è abilitato alla
074700040510      * scannerizzazione errore bloccante
074800040511     c                   If        §Ogdse = *Blanks and v1ccar = 'S'
074900040510     c                   Eval      v1cmsg = msg(32)
075000040510     c                   Eval      *In28 = *On
075100040510     c                   Eval      *In44 = *On
075200040510     c                   Goto      ev1spe
075300040510     c                   EndIf
075400020304     c                   else
075500020304      * non è stata ancora riemessa la videata
075600020304     c                   eval      *in09 = *on
075700040512     c                   If        v1ccar <> 'S'
075800020903     c                   Eval      *In45 = *On
075900040512     c                   EndIf
076000020304     c                   endif
076100020304     c                   endif
076200020304
076300000418      *
076400000418     C                   ELSE
076500000418      * Se sono in FILIALE controllo:
076600000418     C     kspe          chain     fnarb000
076700121019     c                   if        NOT %found  and
076701121019     c                             (V1Ccar <> '3' or v1ccar <> '8')
076800000418     C     kspe          chain     fnblp000
076900000418     c                   endif
077000000418      *  Che la spedizione non esista
077100000418     c                   If        %found
077200000418     c                   movel     MSG(8)        v1CMSG
077300000418     c                   eval      *in28 = *on
077400000418     c                   Else
077500000418      *  Che sia gestibile
077600000418     c     V1Clnp        lookup    l6                                     30
077700000418     c  n30V1Clna        lookup    l6                                     30
077800000418     c                   If        *in30 = *off
077900000418     c                   movel     MSG(9)        v1CMSG
078000000418     c                   eval      *in28 = *on
078100000418     c                   Endif
078200000418     c                   Endif
078300000418      *
078400000317     c                   ENDIF
078500000317      *
078600000321     c                   If        *in28 = *on
078700000320     c                   eval      *in41 = *on
078800000418     c                   goto      ev1spe
078900000317     c                   Endif
079000000317      *
079100000317     c     ev1spe        ENDSR
079200000320      **********************************************************************
079300000418      * DETERMINO LEGAMI BOLLA
079400000320      **********************************************************************
079500000418     C     LEGbol        BEGSR
079600000320      *
079700000418     C                   clear                   wlegame
079800000418      * Non considero i resi come bolle legate se causale richiesta è DANNI
079900110218    3c**                 IF        TASfbr = *blanks  or                         * bolla di reso
080000110218    3c**                           (TASfbr = 'S' and V1Ccar <> 'D')             * bolla di reso
080100110218     c
080200110218     c                   if        v1ccar<>'D' or (tascca<>'6' and tascca<>'2'
080300110218     c                             and tasfbr<>'S' )
080400000418      *
080500000418     c     kspe          chain     fnlbl01l
080600000418     c     Kspe          chain     fnlbl02l
080700000418      *
080800000418     C                   SELECT
080900000418      * Nessun legame
081000000418    3c                   WHEN      NOT %found(fnlbl01l)  and
081100000418    3c                             NOT %found(fnlbl02l)
081200000418      * La spedizione è mamma e figlia
081300000418    3c                   WHEN      %found(fnlbl01l)  and  %found(fnlbl02l)
081400000418     c                   eval      Wlegame = 'X'
081500000418      * La spedizione è mamma originale o ultima figlia
081600000418    3c                   OTHER
081700000418     c                   eval      Wlegame = 'S'
081800000418    1c                   ENDSL
081900000418      *
082000000418     C                   ENDIF
082100000418      *
082200000418     c                   ENDSR
082300000418      **********************************************************************
082400090417      * SCRIVO FILE FNRIG
082500000418      **********************************************************************
082600000418     C     WRTFIRIG      BEGSR
082700000418      *
082800090417     C                   clear                   FNRIG000
082900000320     C                   eval      RIGaas = I20aas
083000000320     C                   eval      RIGlnp = I20lnp
083100000320     C                   eval      RIGnrs = I20nrs
083200000418     C                   eval      RIGnsp = I20nsp
083300000320     C                   eval      RIGlna = I20lna
083400090423      * solo per le bolle firmate che generano bolla di addebito
083500090423     c                   if        i20car = 'F' or i20car = 'S'
083600090423      * imposto il ksc
083700090423     c                   if        v1fmd = 'M'
083800090423     c                   eval      rigksc = v1cccm
083900090423     c                   endif
084000090423     c                   if        v1fmd = 'D'
084100090423     c                   eval      rigksc = v1cccd
084200090423     c                   endif
084300090423      * addebito a carico mittente o destinatario
084400090423     c                   eval      rigadd = v1fmd
084500090423      * se tassazione da tariffa o negata
084600090423      * la scelta è prevista solo per le bolle 'FF'
084700090423      * quindi per le 'FO' imposto fisso da tariffa
084800090423     c                   select
084900090423     c                   when      i20car = 'F' or v1ctar = 'SI'
085000090423     c                   eval      rigtas = 'T'
085100090423     c                   when      v1ctar = 'NO'
085200090423     c                   eval      rigtas = 'N'
085300090423     c                   endsl
085400090423      * n. colli
085500090423     c                   eval      rigncl = 1
085600090423      * codice tariffa
085700090423      * lo imposto se:
085800090423      * bolla franco e paga mittente o se bolla assegnato e paga destinatario
085900090423     c                   if        (v1fmd = 'M' and wtb1 = 'F') or
086000090423     c                             (v1fmd = 'D' and wtb1 = 'A')
086100090423     c                   move      tasctr        rigctr
086200090423     c                   endif
086300090423     c                   endif
086400090423
086500090417     C                   eval      RIGcau = I20car
086600000320     C                   eval      RIGtra = I20tra
086700090417     C                   eval      RIGfil = I20por
086800000320     C                   eval      RIGpru = I20pru
086900090417     C                   eval      RIGdata = I20dim
086901121011     C                   movel     o20tinc       RIGflo
087000090423     C                   movel     wora          RIGora
087001121018     c                   if        v1ccar = '8'
087002121018     C                   move      dateu         RIGdela
087003121018     c                   endif
087100000320      *
087200090417     c                   write     FNRIG000
087300000321     C                   add       1             wrec
087400000320      *
087500000320     c                   ENDSR
087600000317      **************************************************************************
087700000317      * CARICO SCHIERA L6 DA TABELLA £6
087800000317      **************************************************************************
087900000317     C     carl6         BEGSR
088000000317      *
088100000317     C                   CLEAR                   TRUL06DS
088200000317     C                   MOVE      '£6'          D06COD
088300000317     C                   MOVEL(P)  Wfgs          D06key
088400000317     C                   MOVEL(P)  TRUL06DS      KPJBU
088500000317     C                   CALL      'TRUL06R'
088600000317     C                   PARM                    KPJBA
088700000317     C                   MOVEL     KPJBU         TRUL06DS
088800000317      *
088900000317     c                   endsr
088901121018      **************************************************************************
088902121018      * forza scrittura spia
088904121018      **************************************************************************
088905121018     C     ficso         BEGSR
088906121018      *
088907121022     C                   move      riglnp        §fle
088908121018      *richiama override per membro
088909121018     c                   exsr      ovrficso
088910121018      * scrive
088912121019     c                   open      ficso00r
088913121018     C                   clear                   FICSO000
088914121018     C                   MOVEL     'CRSE'        csoCAV
088915121018     C                   Z-ADD     dateu         csoDTV
088916121019     C                   TIME                    csoORV
088917121019     C                   Z-ADD     i20lnp        csoLNP
088918121019     C                   Z-ADD     i20lna        csoLNA
088919121019     C                   Z-ADD     i20lna        csoFEV
088920121019     C                   Z-ADD     i20aas        csoAAS
088921121019     C                   Z-ADD     i20nrs        csoNRS
088922121019     C                   Z-ADD     i20nsp        csoNSP
088923121018     C                   MOVEL     '8'           csoSTA
088924121018     c                   write     FICSO000                             59
088925121019     c                   close     ficso00R
088926121018     c                   endsr
088927121018     C**-------------------------------------------------------------**
088928121018     C** ovrdbf membri  FICSO00r
088929121018     C**-------------------------------------------------------------**
088930121018     C     ovrficso      begsr
088931121018     C** Controllo se esiste membro
088932121018     C                   z-add     49            lung             15 5
088933121019     C                   movea     cmd3          comman           80
088934121018     c                   eval      %subst(comman:43:6) = §mbr
088935121018     C                   CALL      'QCMDEXC'                            34
088936121018     C                   PARM                    comman
088937121018     C                   PARM                    lung
088938121018     C** Se non esiste eseguo addpfm x membro nuovo
088939121018     C                   if        *in34
088940121018     C                   movea     cmd1          comman
088941121018     c                   eval      %subst(comman:43:6) = §mbr
088942121018     C                   call      'QCMDEXC'
088943121018     C                   parm                    comman
088944121018     C                   parm                    lung
088945121018     C                   end
088946121018     C** Esegue in ogni caso OVRDBF del membro desiderato
088947121018     C                   movea     cmd2          comman
088948121018     c                   eval      %subst(comman:43:6) = §mbr
088949121018     C                   CALL      'QCMDEXC'
088950121018     C                   parm                    comman
088951121018     C                   parm                    lung
088952121018     C*
088953121018     C                   endsr
089000000317      *****************************************************************
089100021014      * reperisce outq
089200000317      *****************************************************************
089300021014     C     sroutq        BEGSR
089400021014     c* reperisce il nome della coda di stampa
089500021014     c                   move      'L'           finep
089600021014     c                   move      '0'           tpcoda
089700021014     c                   clear                   outq
089800021014     c                   clear                   libcoda
089900021014     c                   clear                   esito
090000021014     c                   call      'TRULOUTQ'
090100021014     c                   parm                    finep
090200021014     c                   parm                    codpo
090300021014     c                   parm                    tpcoda
090400021014     c                   parm                    outq
090500021014     c                   parm                    libcoda
090600021014     c                   parm                    esito
090700021014     c                   if        esito <> '0' or outq = *blank
090800021014     c                   eval      outq = 'QDKT'
090900021014     c                   endif
091000021014      *
091100021014     c                   endsr
091200021014      *****************************************************************
091300021014      * ROUTINE INIZIALE
091400021014      *****************************************************************
091500021014     C     *INZSR        BEGSR
091600000317      *
091700000317     C     *ENTRY        PLIST
091800000317     C                   PARM                    KPJBA
091900000317      *
092000000320     C                   clear                   TIBS20DS
092100000320      *
092200000418      *  Gestione parametri ricevuti
092300000418     C                   SELECT
092400000418      *  Se non emetto videate ricevo la DS impostata
092500000418     C                   WHEN      %subst(Kpjbu:1:1) = 'N'
092600000320     C                   movel     kpjbu         TIBS20DS
092700000418     c                   eval      Wi20vid = I20vid
092800000418      *  Se emetto la window ricevo la DS impostata (key sped + lna)
092900000418     C                   WHEN      %subst(Kpjbu:1:1) = 'W'
093000040510     c                             or %subst(Kpjbu:1:1) = 'S'
093100000418     C                   movel     kpjbu         TIBS20DS
093200000418     c                   eval      Wi20vid = I20vid
093300040511     c                   Eval      v1ccar = I20Car
093400000418     C                   eval      V1Caas = I20aas
093500000418     C                   eval      V1Clnp = I20lnp
093600000418     C                   eval      V1Cnrs = I20nrs
093700000418     C                   eval      V1Cnsp = I20nsp
093800000418     C                   eval      V1Clna = I20lna
093900000418     C                   eval      *in08 = *on
094000020904     c                   Eval      V1fmd = 'M'
094100040510     c                   Eval      v1cinv = 'SI'
094200040510     c                   Eval      v1ctar = 'SI'
094300040511     c                   Eval      wforza = *Off
094400000418      *  Se emetto il formato pieno non ricevo DS
094500000418     C                   OTHER
094600000418     c                   clear                   Wi20vid
094700000418     C                   ENDSL
094800000317      *
094900000317     C                   Z-ADD     1             CODUT
095000000317     C                   CALL      'X§PARUT'
095100000317     C                   PARM                    UT§DSE0F
095200000317     C                   MOVEL     RAGUT         RSUT
095300000317     C                   MOVEL     REC80         CNCR80
095400000317      *
095500000418      * Imposto P.O. gestione e ne carico la £6
095600000418     C                   IF        SIMFEL = *zeros
095700000317     C                   z-add     46            V1CFGS
095800000317     C                   eval      L6(1) = 046
095900000418      * Non visualizzo "P.O. IN GESTIONE"
096000000418     C                   eval      *in06 = *on
096100000418     C                   eval      *in07 = *on
096200020131     C                   eval      *in04 = *on
096300000418      *
096400000317     C                   ELSE
096500000418      *
096600020523     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
096700020523     C                   IF        simtpp = '2' or simtpp = *blanks
096800020523     C*                  movel     REMFIL        V1CFGS
096900020523     C                   movel     simpou        V1CFGS
097000000418     C                   eval      *in06 = *on
097100000418   X2C                   ELSE
097200000418     C                   movel     SIMFEL        V1CFGS
097300000418    2C                   ENDIF
097400000418      *
097500000317     C                   z-add     v1cfgs        Wfgs
097600000317     C                   clear                   L6
097700000317     C                   exsr      CARL6
097800000317     C                   movea     LIN           L6
097900000317     C                   ENDIF
098000000317
098100000317      * reperisco data e ora
098200000317     C                   TIME                    W0140
098300000320     C                   MOVEL     W0140         WORA
098400000317      * UDATE IN GGMMAAAA
098500000317     C                   MOVE      W0140         WDTGIO
098600000317      * UDATE IN AAAAMMGG
098700000320     C     *eur          MOVEL     WDTGIO        Wdata
098800000320     C     *iso          MOVEL     Wdata         DATEU
098900020507
099000020507      * oggi - 1 anno
099100020507     c                   eval      wdataold = wdata
099200020508     c                   subdur    1:*y          wdataold
099300020507     c     *iso          movel     wdataold      wolddata
099400000317
099500000320      * Apertura files se sede eseguo ovrdspf
099600000317     c                   IF        SIMFEL = *zeros
099700020201     c                   open      Tabel00f
099800020201     c                   open      TITAS30C
099900000417     c                   open      TNCSB03L
100000000418     c                   open      FNLBL01L
100100000418     c                   open      FNLBL02L
100200020903     c                   Open      Titaa30c
100300000317     c                   ELSE
100400000317     c                   open      FNBLP01L
100500000317     c                   open      FNARB01L
100600000317     c                   ENDIF
100700000317      *
100800000317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1  se non sono in sede
100900000317     C                   if        SIMFEL <> 0
101000000317     C                   CLEAR                   TRUL06DS
101100000317     C                   MOVE      '£1'          D06COD
101200000317     C                   MOVEL     SIMFEL        D06KEY
101300000317     C                   MOVEL(P)  TRUL06DS      KPJBU
101400000317     C                   CALL      'TRUL06R'
101500000317     C                   PARM                    KPJBA
101600000317     C                   MOVEL     KPJBU         TRUL06DS
101700000317     C                   MOVEA     LIN           L1
101800000317    2C                   endif
101900000317      *
102000000317     C     kspe          KLIST
102100000317     C                   KFLD                    v1cAAS
102200000317     C                   KFLD                    v1cLNP
102300000317     C                   KFLD                    v1cNRS
102400000317     C                   KFLD                    v1cNSP
102500020903
102600020903     c     Kspea         Klist
102700020903     c                   Kfld                    V1caas
102800020903     c                   Kfld                    V1clnp
102900020903     c                   Kfld                    V1cnrs
103000020903     c                   Kfld                    V1cnsp
103100020903     c                   Kfld                    Wtrc
103200020903
103300020201     C     ktab          KLIST
103400020201     C                   KFLD                    tblkut
103500020201     C                   KFLD                    tblcod
103600020201     C                   KFLD                    tblkey
103700020201     c                   move      1             tblkut
103800020703     c**!!!              move      '3M'          tblcod
103900000317      *
103901121016     C                   EVAL      AutProj = GetProfiloUtenteProj(knmus)
104000000317     C                   ENDSR
104001121016     P*--------------------------------------------------
104002121016     P* Procedure name: GetProfiloUtenteProj
104003121016     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
104004121016     P* Returns:        Profilo utente in Proj.
104005121016     P* Parameter:      priProfiloUtente => Profilo Utente
104006121016     P*--------------------------------------------------
104007121016     P GetProfiloUtenteProj...
104008121016     P                 B
104009121016     D GetProfiloUtenteProj...
104010121016     D                 PI             1A
104011121016     D  priProfiloUtente...
104012121016     D                               10A
104013121016
104014121016     D AutProj         S              1A   STATIC
104015121016
104016121016      /FREE
104017121016
104018121016       CLEAR AutProj;
104019121016
104020121016       EXEC SQL
104021121016         SELECT CASE
104022121016                WHEN utnAnn = '1' THEN 'A'
104023121016                WHEN utnGrp = 999 THEN 'D'
104024121016                ELSE 'S'
104025121016                END
104026121016           INTO :AutProj
104027121016           FROM anutn00f
104028121016           WHERE utnPrf = :priProfiloUtente
104029121016       ;
104030121016
104031121016       SELECT;
104032121016         WHEN sqlCode < *ZERO;
104033121016           DUMP(A);
104034121016           Autproj = '?';
104035121016       ENDSL;
104036121016
104037121016       RETURN autproj;
104038121016
104039121016      /END-FREE
104040121016     P GetProfiloUtenteProj...
104041121016     P                 E
104100000317      *---------------------------------------------------------------------------------------------
104101121018**         CMD1
104102121019ADDPFM FILE(FICSO00R)                MBR(M000000)
104103121018**         CMD2
104104121019OVRDBF FILE(FICSO00R)                MBR(M000000)
104105121018**         CMD3
104106121019CHKOBJ  OBJ(FICSO00R) OBJTYPE(*FILE) MBR(M000000)
104200000317** MSG  Lungh. 78                                                            *
104300000418Immettere la linea                                                            1
104400000418Linea inesistente o annullata                                                 2
104500000418Richiesta di rigenerazione spedizione eseguita                                3
104600000418Inserire un numero spedizione valido                                          4
104700000418Spedizione inesistente, impossibile rigenerare !                              5
104800000418Per la spedizione esiste il C/Assegno, impossibile rigenerare !               6
104900000419Spedizione precedente il settembre 1999, impossibile rigenerare !             7
105000000418La spedizione esiste, impossibile rigenerare !                                8
105100000418Spedizione in gestione ad altro p.o.                                          9
105200000418Causale richiesta non valida !                                                10
105300000418Impossibile rigenerare bolle legate per la gestione C/Assegni omessi !        11
105400000419Bolla legata: per i Danni richiedere l'ultima figlia o la mamma originale !   12
105500000419Bolla legata con mamma originale precedente il 9/1999, impossibile rigenerare 13
105600020221Si possono rigenerare bolle con causale c/a omessi solo se si è in sede.
105700020201Utente non abilitato alla rigenerazione bolla con causale contrassegni omessi.
105800020221Non è ammessa la rigenerazione di una bolla POSTA con causale c/a omessi.
105900020221Non è ammessa la rigenerazione di una bolla AUTOMATICA con causale c/a omessi.
106000020221Può rigenerare la bolla solo l'utente della fililiale di partenza.
106100020304Bolla legata: per Bolla Firmata richiedere l'ultima figlia o mamma originale! 19
106200020304Immettere il p.o. che effettua la richiesta                                   20
106300020304P.o. immesso errato                                                           21
106400020305Si possono rigenerare bolle con causale 'F' solo se si è in sede              22
106500020305Funzione non ancora attiva                                                    23
106600020305Non è ammessa la rig. di una bolla NON CONSEGNATA con causale Bolla firmata!  24
106700020507Richiesta spedizione più vecchia del limite per l'archiviazione               25
106800020703Non è ammessa la rigenerazione di una bolla di RECUPERO                       26
106900020903Non ammessa la rig. di una bolla con mitt. e dest. non codificati!            27
107000020903Attenzione il mittente non è codificato non si può addebitare                 28
107100020903Attenzione il destinatario non è codificato non si può addebitare             29
107200020903Immettere il falg a chi addebitare                                            30
107300040511Utilizzare F19=Immagine per richiesta bolla firmata.                          31
107400040511Il p.o. non è abilitato alla scannerizzazione.                                32
107500040511Bolla già rigenerata in data xx/xx/xxxx. Enter x Forzare!                     33
