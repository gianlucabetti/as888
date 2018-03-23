000100090108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300020305
000400020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000500020305
000600090108     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000700090107     F                                     INFDS(TAR)
000800090406     Ffifgt03l  if   e           k disk
000900090406
001000020311     Ffifpt03l  if   e           k disk
001100020304     Ffifpd01l  if   e           k disk
001200021203     Ffiapd01l  if   e           k disk
001300020305     Ftabel00f  if   e           k disk
001400080918     Fancdp01l  if   e           k disk
001500080918     Fanfrn01l  if   e           k disk
001600080915     FanRCO98J  iF   E           K DISK
001700090428     Fqsysprt   O    F  132        PRINTER INFDS(PRTFBK) prtctl(prtds)
001800090428     f                                     usropn
001900090428     d prtFBK          ds
002000090428     D ODP_TYPE               81     82                                         * ODP Type
002100090428     D FILE_NAME              83     92                                         * File name
002200090428     D LIBRARY                93    102                                         * Library name
002300090428     D SPOOL_FILE            103    112                                         * Spool file name
002400090428     D SPOOL_LIB             113    122                                         * Spool file lib
002500090428     D SPOOL_NUM             123    124I 0                                      * Spool file num
002600090428     D RCD_LEN               125    126I 0                                      * Max record len
002700090428     D KEY_LEN               127    128I 0                                      * Max key len
002800090428     D MEMBER                129    138                                         * Member name
002900090428     D TYPE                  147    148I 0                                      * File type
003000090428     D ROWS                  152    153I 0                                      * Num PRT/DSP rows
003100090428     D COLUMNS               154    155I 0                                      * Num PRT/DSP cols
003200090428     D NUM_RCDS              156    159I 0                                      * Num of records
003300090428     D ACC_TYPE              160    161                                         * Access type
003400090428     D DUP_KEY               162    162                                         * Duplicate key?
003500090428     D SRC_FILE              163    163                                         * Source file?
003600090428     D VOL_OFF               184    185I 0                                      * Vol label offset
003700090428     D BLK_RCDS              186    187I 0                                      * Max rcds in blk
003800090428     D OVERFLOW              188    189I 0                                      * Overflow line
003900090428     D BLK_INCR              190    191I 0                                      * Blk increment
004000090428     D FLAGS1                196    196                                         * Misc flags
004100090428     D REQUESTER             197    206                                         * Requester name
004200090428     D OPEN_COUNT            207    208I 0                                      * Open count
004300090428     D BASED_MBRS            211    212I 0                                      * Num based mbrs
004400090428     D FLAGS2                213    213                                         * Misc flags
004500090428     D OPEN_ID               214    215                                         * Open identifier
004600090428     D RCDFMT_LEN            216    217I 0                                      * Max rcd fmt len
004700090428     D CCSID                 218    219I 0                                      * Database CCSID
004800090428     D FLAGS3                220    220                                         * Misc flags
004900090428     D NUM_DEVS              227    228I 0                                      * Num devs defined
005000090428      *
005100090428     D CUR_LINE              367    368I 0                                      * Current line num
005200090428     D CUR_PAGE              369    372I 0                                      * Current page cnt
005300090428      ***
005400090428      *
005500090428     d prtds           ds
005600090428     d   spab                         3  0
005700090428     d   spaa                         3  0
005800090428     d   skab                         3  0
005900090428     d   skaa                         3  0
006000090428     d   line                         3  0
006100090428     d savpdr          s                   like(apdpdr)
006200090428      *
006300080915     D DVCOBADS      E DS
006400080915     D                                     INZ
006500090107     D TAR             DS
006600090107     D  tgtNRR               397    400B 0
006700080915     D CBAIBANDS     E DS
006800020304     dkpjba          e ds
006900080915     ddgpi           e ds
007000090113     dficn05ds       e ds
007100020305     dds8a           e ds
007200020305     dds1z           e ds
007300020305     ddstr           e ds
007400090504     d costt           s             33
007500020304
007600020304     dparam            ds
007700020308     D lancio                         1
007800020304     d codpadr                        7s 0
007900020308     D codpadr2                       7s 0
008000020304     d alladat                        8s 0
008100090424     d pardtcerta                     8s 0
008200090428     d parmNRC                        7s 0
008300090428     d parmFIL                        3s 0
008400090428     D OUTQOVR                       10
008500090428     D errore                         1
008600091021     D simulata                       1
008700020304
008800020402     d modo            s              8
008900020114     d base            s              1
009000080305     d Data_testata    s               d   datfmt(*iso)
009100090108     d savprg          s                   like(tgtprg)
009200090108     d fgtdatas        s                   like(fgtddt)
009300080915     d fgtddts         s                   like(fgtddt)
009400020305     d fgtdsts         s                   like(fgtdst)
009500020305     d fgtduvs         s                   like(fgtduv)
009600020305     d fgtdcns         s                   like(fgtdcn)
009700020305     d fgtdtss         s                   like(fgtdts)
009800020305     d fptarls         s              7  2
009900020305     d fptarfs         s              7  2
010000020305     d fptaros         s              7  2
010100020305     d fptatas         s             10  3
010200020305     d fpdsgls         s              9  0
010300020305     d fpditrs         s             10  3
010400020305     d fpdmins         s             10  3
010500020305     d fpdmaxs         s             10  3
010600020116      * ds di comodo per impostare campo filler in caso di presenza BONUS
010700020116     d dsbonus         ds
010800020116     d   tpbonus                      1
010900020116     d   pabonus                      3  0
011000020116     d   sabonus                      3  0
011100020118     d   pmbonus                      1
011200020305     d dataiso         s               d   datfmt(*iso)
011300020305     d dataeur         s               d   datfmt(*eur)
011400020305     D TPS             S              1    DIM(50)                              TIPO PRESTAZIONE
011500020305     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
011600020305     D CDT             S              3    DIM(50)                              COD.TAR. DEL TIPO PR
011700020305     D TSM             S              1    DIM(50)                              TIPO SOMMA PESI
011800020305     D DSM             S             15    DIM(50)                              DESCR.TIPO SOMMA PES
011900090428     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              OVRPRTF
012000090420     ***************************************************************************
012100090420     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
012200090420     D ESITO_OK...
012300090420     D                 C                   0
012400090420     ***************************************************************************
012500090420     **
012600090420     ** Dichiarazione prototipi procedure esterne.
012700090420     **
012800090420     ***************************************************************************
012900090420      /DEFINE DFTACTGRP_YES
013000090420     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
013100090420      /UNDEFINE DFTACTGRP_YES
013200090420     ***************************************************************************
013300090420     **
013400090420     ** Definizione strutture dati.
013500090420     **
013600090420      **************************************************************************
013700090420     D tibsSocI0...
013800090420     D               E DS                  QUALIFIED
013900090420     D                                     INZ
014000090420     D tibsSocO0...
014100090420     D               E DS                  QUALIFIED
014200090420     D                                     INZ
014300090420     ***************************************************************************
014400090420     **
014500090420     ** Definizione variabili modulo/programma.
014600090420     **
014700090420     ***************************************************************************
014800090420     D prmRqsOpCode...
014900090420     D                 S             10A
015000090420     D prmRpyOpCode...
015100090420     D                 S             10A
015200090420     D prmRpyIdMsg...
015300090420     D                 S             10I 0
015400090420     D prmRqsFormato...
015500090420     D                 S             10A
015600090420     D prmRqsDataSize...
015700090420     D                 S             10I 0
015800090420     D prmRpyFormato...
015900090420     D                 S             10A
016000090420     D prmRpyDataSize...
016100090420     D                 S             10I 0
016200090420     D Wsocg           S                   like(apdCSF)
016300090420     D Wsocgd          S                   like(desbrev)
016400090420
016500000000     C*---------------------------------------------------------------*
016600090420     ** Inizializzo il programma.
016700090420     C                   CALL      'TIBSSOCR'
016800090420     C                   PARM      'INIT'        prmRqsOpCode
016900090420     C                   PARM                    prmRpyOpCode
017000090420     C                   PARM                    prmRpyIdMsg
017100020304      *
017200020304     c                   z-add     codpadr       kpdr
017300091021     c                   movel     simulata      ksml
017400020308     c
017500021203     c     kapd          setll     fiapd01l
017600020308     c                   do        *hival
017700021203     c     apdtip        reade     fiapd01l                               99
017800020308     c   99              leave
017900020308     c                   if        apdpdr > codpadr2
018000020308     c                   leave
018100020308     c                   endif
018200090428      *
018300090428      *  A rottura di AUTista
018400090428     c                   if        apdpdr <> savpdr
018500090428     c                   eval      savpdr = apdpdr
018600090428      *
018700090428     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
018800090428     C                   close     qsysprt
018900090428     C                   MOVEL(p)  CMD(1)        COMMAN160       160
019000090428     C                   MOVE      CMD(2)        COMMAN160       160
019100090428      *
019200090428      *  se deve essere indirizzata ad una specifica coda di stampa
019300090428     c                   if        OUTQovr <> *blank
019400090428     c                   eval      COMMAN160 = %trim(COMMAN160) + ' OUTQ(' +
019500090428     c                             %Trim(OUTQovr) + ')'
019600090428     c                   end
019700090428      *
019800090428     C                   Z-ADD     160           LUNG             15 5
019900090428     C                   CALL      'QCMDEXC'
020000090428     C                   PARM                    COMMAN160
020100090428     C                   PARM                    LUNG
020200090428     C                   OPEN      qsysprt
020300091021     c  N19              movel     'P R O V A'   novealfa          9
020400091021     c   19              movel     'SIMULATA '   novealfa
020500090428     c                   endif
020600090428      *
020700020308     c                   z-add     apdpdr        kpdr
020800090408     c                   clear                   tlzrscs          35
020900090408     c                   clear                   tlzpdrs           7 0
021000080915     C*---------- RICERCA DITTA :
021100080915     c                   clear                   desbrev          20
021200090420      ***
021300090420     c                   eval      wsocG = apdCSF
021400090420     c                   exsr      Decod_societa
021500090420     c                   eval      desbrev = wsocGd
021600090420      ***
021700080915     c* fornitore
021800080915     c                   movel     *all'0'       rcoksc
021900080915     c                   move      apdksc        rcoksc
022000080915     C     Krco          CHAIN     anrco98j                           20
022100080915     c                   if        not *in20
022200090408      *
022300090408      *  qui reimposta i 2 campi se li ha trovati su PROJ
022400090408     c                   eval      tlzrscs = sogDES
022500090408     c                   eval      tlzpdrs = apdKsc
022600090408      *
022700080915     c                   end
022800080918     c* condizioni di pagamento
022900080918     c                   movel     *all'_'       cdpdescr
023000080918     C     Kfrn          CHAIN     anfrn01l
023100080918     c                   if        %found(anfrn01l)
023200080918     C     Kcdr          CHAIN     ancdp01l
023300080918     c                   end
023400080915     c* reperisco IBAN
023500080915     C                   RESET                   DVCOBADS
023600080915     C                   RESET                   CBAIBANDS
023700080915
023800080915     C                   EVAL      CoBSoc = apdcsf
023900080915     C                   EVAL      CoBCOF = 'F'
024000080915     C                   EVAL      CoBKsc = rcoksc
024100080915     C                   CALL      'DVCOBA'
024200080915     C                   PARM                    DVCOBADS
024300080915A0846C                   PARM                    CBAIBANDS
024400080915     C                   IF        CoBErr = *OFF
024500080915     c                   end
024600020308     c                   exsr      elabora
024700020308     c                   enddo
024800090113      *
024900020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
025000020308     C                   close     qsysprt
025100090428     C                   MOVEL(p)  CMD(5)        COMMAN80         80
025200020308     C                   Z-ADD     80            LUNG             15 5
025300020308     C                   CALL      'QCMDEXC'
025400090428     C                   PARM                    COMMAN80
025500020308     C                   PARM                    LUNG
025600090420     C*
025700090420     ** Chiudo il programma.
025800090420     C                   CALL      'TIBSSOCR'
025900090420     C                   PARM      'FINALIZE'    prmRqsOpCode
026000090420     C                   PARM                    prmRpyOpCode
026100090420     C                   PARM                    prmRpyIdMsg
026200020304
026300090429     c                   eval      kpjbu = param
026400020308     c                   seton                                        lr
026500020308      *--------------------------------------------------
026600020308     c     elabora       begsr
026700020308      *--------------------------------------------------
026800090113      *
026900090113     c                   clear                   TGT_trovato       1
027000090113     c                   clear                   Kmetri            5 0
027100090403     c                   clear                   tmp_caric         4 0
027200090403     c                   clear                   tmp_scar          4 0
027300090113      *
027400090107     c                   eval      savprg = *hival
027500090109     c                   clear                   prezzo
027600090113      *
027700020411     c                   do        *hival
027800090107     c* cerco il progressivo
027900090107     c     ktgt          setll     fitgt01l
028000090113      *
028100090107     c                   do        *hival
028200090107     c     ktgt          reade     fitgt01l
028300090113      *
028400090107     c                   if        %eof(fitgt01l)
028500090107     c                   leave
028600090107     c                   end
028700090113      *
028800090109     c* escludo le annullate
028900090109     c                   if        tgtatb <> ' '
029000090109     c                   iter
029100090109     c                   end
029200090113      *
029300090107      * non considero le tariffe che risultano fuori periodo
029400090107     c                   if        tgtddt > alladat or  tgtdst < alladat
029500090107     c                   iter
029600090107     c                   end
029700090113      *
029800090113     c                   move      'S'           TGT_trovato       1
029900090113      *
030000090107     c                   eval      savprg = tgtprg
030100090107     c                   leave
030200090107     c                   enddo
030300090113      **
030400090113     c                   if        tgtKM > 0
030500090113     c                   eval      Kmetri = tgtKM
030600090113     c                   end
030700090403     c                   if        TGTTMPC > 0
030800090403     c                   eval      tmp_caric = TGTTMPC
030900090403     c                   end
031000090403     c                   if        TGTTMPS > 0
031100090403     c                   eval      tmp_scar  = TGTTMPS
031200090403     c                   end
031300090113      **
031400090109     c* prezzo iniziale carburante legge 133
031500090109     c                   if        tgtpcar <> 0
031600090109     c                   eval      prezzo = tgtpcar
031700090109     c                   else
031800090113      **
031900090113     c                   clear                   ficn05ds
032000090113     c                   call      'FICN05R'
032100090113     c                   parm                    ficn05ds
032200090113      **
032300090113      ** se non ha agganciato la tabella imposta fisso il prezzo
032400090113     c                   if        o05err = 'E'
032500090113     c                   eval      prezzo = 1,518
032600090113     c                   else
032700090113      **  altrimenti lo prende dalla tabella
032800090113     c                   movel     o05gpi        dgpi
032900090113     c                   z-add     d§gpigpi      prezzo            5 3
033000090113     c                   end
033100090113      **
033200090109     c                   end
033300020304      * per giornata
033400020304     c                   move      'G'           ktsr
033500011026     c                   exsr      fifgt
033600090113      *
033700090113      * Se problemi con Supertestata non deve stampare altri FGT
033800090113     c                   if        salta_altriFGT = *blank
033900020304      * per consegna
034000020304     c                   move      'C'           ktsr
034100020304     c                   exsr      fifgt
034200090113     c                   ENDif
034300090113      *
034400090113      * Se problemi con Supertestata non deve stampare altri FGT
034500090113     c                   if        salta_altriFGT = *blank
034600020304      * per ritiro
034700020304     c                   move      'R'           ktsr
034800020304     c                   exsr      fifgt
034900090113     c                   ENDif
035000090113      **
035100090113     c*  Solo in presenza della supertestata stampa la "Clausola" aggiuntiva
035200090113     c*   x esprimere i KM.
035300090113     c                   setoff                                       33
035400090113     c                   if        TGT_trovato ='S'
035500090113     c                   seton                                        33
035600090113     c                   end
035700020304
035800020308      * stampo il fondo pagina solo se ho trovato qualcosa per il codice
035900020308      * che sto elaborando e ripristino la variabile per l'intestazione del
036000020308      * successivo
036100020308     c                   if        prima <> *blank
036200090428      *
036300090428      * deve stampare la nuova pagina con il bollo
036400090428     c                   seton                                        66
036500090428      *
036600090428      *   MA SENZA LA FIRMA
036700090428     C                   close     qsysprt
036800090428     C                   MOVEL(p)  CMD(3)        COMMAN160       160
036900090428     C                   MOVE      CMD(4)        COMMAN160       160
037000090428      *
037100090428      *  se deve essere indirizzata ad una specifica coda di stampa
037200090428     c                   if        OUTQovr <> *blank
037300090428     c                   eval      COMMAN160 = %trim(COMMAN160) + ' OUTQ(' +
037400090428     c                             %Trim(OUTQovr) + ')'
037500090428     c                   end
037600090428      *
037700090428     C                   Z-ADD     160           LUNG             15 5
037800090428     C                   CALL      'QCMDEXC'
037900090428     C                   PARM                    COMMAN160
038000090428     C                   PARM                    LUNG
038100090428     C*
038200090428     C*  Riapre il PRINTER senza la FIRMA
038300090428     C                   open      qsysprt
038400091021     c  N19              movel     'P R O V A'   novealfa          9
038500091021     c   19              movel     'SIMULATA '   novealfa
038600090428     C*
038700020308     c                   clear                   prima             1
038800020308     c                   endif
038900090428     C*
039000020412     c                   z-add     0             page
039100020411      *forzatura uscita
039200080915     c                   if        lancio = 'P' or lancio = 'R' or *in71
039300090420     c                   leave
039400080915     c                   endif
039500090428     C*
039600090420     c                   seton                                        71
039700020411     c                   enddo
039800090428     C*
039900020411     c                   setoff                                       71
040000020308
040100020308     c                   endsr
040200020304
040300990113     c*-------------------------------------------------
040400011026     c     fifgt         begsr
040500990113     c*-------------------------------------------------
040600090114     c                   clear                   salta_altriFGT    1
040700091021     c                   movel     simulata      ksml
040800080305      * data x allegato in vigore al
040900080305     c                   exsr      data_in_vigore
041000080305      *
041100990113     c                   setoff                                           99
041200020305     c     kfgt          setll     fifgt03l
041300090113      *
041400981029     c                   do        *hival
041500020305     c     kfgt          reade     fifgt03l                               99
041600981029     c   99              leave
041700090113      *
041800090109     c* solo il progressivo della supertestata se l'ho trovata
041900090109     c                   if        fgtprg <>savprg and savprg <> *hival
042000090107     c                   iter
042100090107     c                   end
042200090113      *
042300080918      * non considero le tariffe che risultano fuori periodo
042400020305     c                   if        fgtddt > alladat or  fgtdst < alladat
042500020114     c                   iter
042600080225     c                   else
042700090113      *
042800080225      *giro le date per la stampa
042900080305     c                   move      Data_testata  dataeur
043000080305     c                   move      dataeur       fgtddts
043100080225
043200080225     c                   end
043300090113
043400020308      *trovata tariffa valida per il padroncino stampo intestazione
043500020308     c                   if        prima = *blank
043600090428      *
043700090428      *  prima pagina con i BOX per bollo e data certa
043800090428     c                   seton                                        66
043900020308     c                   except    testata
044000090424     c                   setoff                                       66
044100020308     c                   move      'X'           prima
044200020308     c                   endif
044300090113      **
044400020305     c                   z-add     1             x                 3 0
044500020306     c     fgttsr        lookup    tps(x)                                 91
044600020306     c   91              movel     dsr(x)        destsr           15
044700090113      *
044800020305      *giro le date per la stampa
044900080305     c                   move      Data_testata  dataeur
045000080305     c                   move      dataeur       fgtddts
045100020305
045200020306     c                   if        line > 59
045300090428     c                   exsr      test_pagina
045400020306     c                   except    testata
045500020306     c                   endif
045600090113      ****
045700090113     c                   clear                   salta_altriFGT    1
045800090113      *>>>>>>>>>
045900090113      *  se già la tariffa doveva avere la supertestata (quindi >= 1/2/2009)
046000090113      * occorre controllare e segnalare
046100090113      *  se non ci sono i KM della supertestata o se non c'è la supertestata.
046200090113      *
046300090113     c                   if        fgtDDT >= 20090201
046400090427     c                   IF          TGT_trovato = *blank
046500090113      **
046600090113      ** per non stampare in seguito il piede
046700090113     c                   clear                   prima
046800090113      ****
046900090113      **   se non c'è la supertestata dopo la data 1/2/2009 c'è stato un problema
047000090113      **   tecnico nella conversione o nell'inserimento della nuova tariffa, quindi,
047100090113      **   deve essere segnalato.
047200090113      ****
047300090113      * Non c'è la superTestata
047400090113     c                   if        TGT_trovato = *blank
047500090113     c                   except    NO_SuperT
047600090429     c                   move      'E'           errore
047700090113     c                   move      'S'           salta_altriFGT    1
047800090113     c                   LEAVE
047900090113     c                   else
048000090113     c                   end
048100090113      *
048200090113     c                   end
048300090113     c                   end
048400090113      *>>>>>>>>>
048500090113      ****
048600020304     c                   except    testa
048700020304     c                   exsr      fifpt
048800090113      ***
048900020304     c                   enddo
049000020304
049100020304     c                   endsr
049200080305     c*-------------------------------------------------
049300080305     c     data_in_vigorebegsr
049400080305     c*-------------------------------------------------
049500080305      *  esegue un ciclo prima di stampare x prendere la
049600080305      *  data più recente da mostrare in stampa
049700080305      *
049800080305     c                   clear                   Data_testata
049900080305     c                   do        3             tsr               3 0
050000080305     c                   if        tsr = 1
050100080305     c                   eval      TipSer ='G'
050200080305     c                   elseIF    tsr = 2
050300080305     c                   eval      TipSer ='C'
050400080305     c                   elseIF    tsr = 3
050500080305     c                   eval      TipSer ='R'
050600080305     c                   endIF
050700080305     c     kfgtsr        setll     fifgt03l
050800080305     c                   do        *hival
050900080305     c     kfgtsr        reade     fifgt03l                               99
051000080305     c   99              leave
051100090109     c* solo il progressivo della supertestata se l'ho trovata
051200090109     c                   if        fgtprg <>savprg and savprg <> *hival
051300090109     c                   iter
051400090109     c                   end
051500080305      *
051600080305      * non considero le tariffe che risultano fuori periodo
051700080305     c                   if        fgtddt > alladat or  fgtdst < alladat
051800080305     c                   iter
051900080305     c                   else
052000080305      *
052100080305      * giro le date per la stampa
052200080305     c                   if        fgtddt > 0
052300080305     c                   move      fgtddt        dataiso
052400080305     c                   if        dataiso > Data_testata
052500080305     c                   eval      Data_testata = dataiso
052600080305     c                   end
052700080305     c                   end
052800080305     c                   end
052900080305     c                   endDO
053000080305
053100080305     c                   endDO
053200080305
053300080305     c                   endsr
053400990113     c*-------------------------------------------------
053500011026     c     fifpt         begsr
053600990113     c*-------------------------------------------------
053700020109     c
053800020304     c                   setoff                                           98
053900020311     c     kfpt          setll     fifpt03l
054000020304     c                   do        *hival
054100020311     c     kfpt          reade     fifpt03l                               98
054200020304     c   98              leave
054300020304     c                   if        fptatb <> *blank
054400020304     c                   iter
054500020304     c                   endif
054600020117
054700020305      *descrizione particolarità
054800020305     c                   clear                   key
054900020305     c                   movel     '8A'          cod
055000020305     c                   movel     fptctd        key
055100020305     c     ktab          chain     tabel                              95
055200020305     c  n95              movel     tbluni        ds8a
055300020402     c  n95              movel     §8ades        desctd           25
055400020305     c   95              clear                   desctd
055500020305      *desrizione tipo tariffa
055600020305     c                   clear                   key
055700020305     c                   movel     'TR'          cod
055800020305     c                   movel     fpttpg        key
055900020305     c     ktab          chain     tabel
056000020305     c  n95              movel     tbluni        dstr
056100020305     c  n95              movel     §trdes        destpg           15
056200020305     c   95              clear                   destpg
056300020305
056400020307     c                   clear                   dsbonus
056500020307     c     fptctd        comp      'BON'                                  82
056600020307     c   82              movel     fptflr        dsbonus
056700020307
056800020311     c     fpttpg        comp      'V'                                    83
056900020311     c                   select
057000020311     c                   when      fpttsm = 'G'
057100020311     c                   movel(p)  'Giornata'    destsm           11
057200020311
057300020311     c                   when      fpttsm = 'P'
057400020311     c                   movel(p)  'Prestazione' destsm
057500020311
057600020311     c                   when      fpttsm = 'T'
057700020311     c                   movel(p)  'Stop'        destsm
057800020311
057900020311     c                   when      fpttsm = 'S'
058000020311     c                   movel(p)  'Spedizione'  destsm
058100020311
058200020311     c                   endsl
058300020311
058400020313     c     fptctd        comp      'BAC'                                  77
058500020313     c  n77fptctd        comp      'BAR'                                  77
058600091202     c                   z-add(h)  fptata        fptatas
058700020311     c     fptatas       comp      0                                      78
058800020305     c                   z-add     fptarl        fptarls
058900020305     c                   z-add     fptarf        fptarfs
059000020305     c                   z-add     fptaro        fptaros
059100020306     c
059200020306     c     fptarls       comp      0                                  81
059300020306     c  n81fptarfs       comp      0                                  81
059400020306     c  n81fptaros       comp      0                                  81
059500020306     c  n81fptrpv        comp      0                                  81
059600020305
059700020306     c                   if        line > 60
059800090428     c                   exsr      test_pagina
059900020305     c                   except    testata
060000020306     c                   endif
060100020117     c                   except    detta
060200020117
060300020304     c                   exsr      fifpd
060400020304     c                   enddo
060500011026
060600990113     c                   endsr
060700990113     c*-------------------------------------------------
060800011026     c     fifpd         begsr
060900990113     c*-------------------------------------------------
061000020114
061100020304     c                   setoff                                           97
061200020305     c     kfpd          setll     fifpd01l
061300020304     c                   do        *hival
061400020305     c     kfpd          reade     fifpd01l                               97
061500020304     c   97              leave
061600020304     c                   if        fpdatb <> *blank
061700020304     c                   iter
061800020304     c                   endif
061900020304
062000091202     c                   z-add(h)  fpditr        fpditrs
062100091202     c                   z-add(h)  fpdmin        fpdmins
062200091202     c                   z-add(h)  fpdmax        fpdmaxs
062300020305     c                   z-add     fpdsgl        fpdsgls
062400020305
062500020306     c                   if        line > 60
062600090428     c                   exsr      test_pagina
062700020305     c                   except    testata
062800020304     c                   else
062900020305     c                   except    dettab
063000020304     c                   endif
063100020304
063200020304     c                   enddo
063300020304
063400990113     c                   endsr
063500981029      *
063600011026     c*-------------------------------------------------
063700011026     c     *inzsr        begsr
063800011026     c*-------------------------------------------------
063900020304     c     *entry        plist
064000020304     c                   parm                    kpjba
064100020304     c                   movel     kpjbu         param
064200020304
064300020304     c                   z-add     codpadr       kpdr
064400091021     c                   movel     simulata      ksml
064500091021     c                   if        simulata ='S'
064600091021     c                   seton                                        19
064700091021     c                   end
064800090424     c                   seton                                        66
064900020304     c
065000090504     c                   eval      costt = 'ALLEGATO TARIFFARIO IN VIGORE DAL'
065100090504     c
065200090108     c     ktgt          klist
065300020304     c                   kfld                    kpdr
065400020304     c                   kfld                    ksml
065500090108     c     kfgt          klist
065600090108     c                   kfld                    kpdr
065700090108     c                   kfld                    ksml
065800090108     c                   kfld                    ktsr
065900011026
066000080305     c     kfgtsr        klist
066100080305     c                   kfld                    kpdr
066200080305     c                   kfld                    ksml
066300080305     c                   kfld                    TipSer            1
066400020313
066500020304     c     kfpt          klist
066600020304     c                   kfld                    fgtpdr
066700020304     c                   kfld                    fgtsml
066800020304     c                   kfld                    fgttsr
066900020304     c                   kfld                    fgtctr
067000020304     c                   kfld                    fgtprg
067100020117
067200020304     c     kfpd          klist
067300020304     c                   kfld                    fptpdr
067400020304     c                   kfld                    fptsml
067500020304     c                   kfld                    fpttsr
067600020304     c                   kfld                    fptctr
067700020304     c                   kfld                    fptprg
067800020304     c                   kfld                    fptctd
067900020304
068000020305     C     ktab2         klist
068100020305     C                   kfld                    codut             1 0
068200020305     C                   kfld                    cod
068300020305
068400020305     C     ktab          klist
068500020305     C                   kfld                    codut
068600020305     C                   kfld                    cod
068700020305     C                   kfld                    key
068800080915
068900080915     C     Krco          KLIST
069000080915     C                   KFLD                    apdcsf
069100080915     C                   KFLD                    rcosnatura
069200080915     C                   KFLD                    rcoksc
069300080915     c                   eval      rcosnatura = 'F'
069400080915
069500080918     C     Kfrn          KLIST
069600080918     C                   KFLD                    apdcsf
069700080918     C                   KFLD                    rcoksc
069800080918     C     Kcdr          KLIST
069900080918     C                   KFLD                    apdcsf
070000080918     C                   KFLD                    FRNCONDPAG
070100080918
070200021203     c     kapd          klist
070300021203     c                   kfld                    apdtip
070400021203     c                   kfld                    kpdr
070500021203     c                   movel     'A'           apdtip
070600020402
070700020305     c                   z-add     1             codut
070800020313     C     *LIKE         DEFINE    TBLCOD        COD
070900020305     C     *LIKE         DEFINE    TBLKEY        KEY
071000020305     C     *LIKE         DEFINE    fgtpdr        kpdr
071100020305     C     *LIKE         DEFINE    fgtsml        Ksml
071200020305     C     *LIKE         DEFINE    fgttsr        Ktsr
071300020305
071400020218     C                   time                    w0120            14 0
071500020218     C                   move      w0120         wdat              8 0
071600020308      * data da impostare su data stampa e convalida se lancio = 'C'
071700020308     c                   move      wdat          dataeur
071800020308     c                   move      dataeur       dataiso
071900020308     c                   move      dataiso       dataoggi          8 0
072000020308      * data da elaborare
072100020304     c                   move      alladat       dataiso
072200020304     c                   move      dataiso       dataeur
072300020304     c                   move      dataeur       alladata          8 0
072400011026
072500020117     c                   z-add     99            line
072600020305     c                   move      *all'_'       separa          100
072700020402     c                   select
072800020402     c                   when      lancio = 'C'
072900020411     c                   clear                   modo
073000020411     c                   seton                                        70
073100020402     c                   when      lancio = 'R'
073200090417     c******             eval      modo = 'Ristampa'
073300020402     c                   when      lancio = 'P'
073400090417     c*******            eval      modo = 'Prova   '
073500020402     c                   endsl
073600020304
073700020305     C****  TIPI PRESTAZIONE E DESCRIZIONI: TPS / DSR / CDT  ****
073800020305     C                   Z-ADD     1             X
073900020305     C                   MOVEL     '1Z'          COD
074000020305     C     KTAB2         SETLL     TABEL
074100020305     C     KTAB2         READE     TABEL                                  32
074200020305     C*
074300020305     C     *IN32         DOWEQ     *OFF
074400020305     C     TBLFLG        IFEQ      ' '
074500020305     C                   MOVEL     TBLKEY        TPS(X)
074600020305     C                   MOVEL     TBLUNI        DS1Z
074700020305     C                   MOVEL     §1ZDES        DSR(X)
074800020305     C* CODICE TARIFFA DEL TIPO PRESTAZIONE
074900020305     C                   MOVEL     §1ZCTR        CDT(X)
075000020305     C                   ADD       1             X
075100020305     C                   ENDIF
075200020305     C     KTAB2         READE     TABEL                                  32
075300020305     C                   ENDDO
075400020305     C*
075500020305     C****  TIPI SOMMA PESI E DESCRIZIONI: TSM / DSM  ****
075600020305     C                   Z-ADD     1             X
075700020305     C                   MOVEL     '1N'          COD
075800020305     C     KTAB2         SETLL     TABEL
075900020305     C     KTAB2         READE     TABEL                                  32
076000020305     C*
076100020305     C     *IN32         DOWEQ     *OFF
076200020305     C     TBLFLG        IFEQ      ' '
076300020305     C                   MOVEL     TBLKEY        TSM(X)
076400020305     C                   MOVEL     TBLUNI        DSM(X)
076500020305     C                   ADD       1             X
076600020305     C                   ENDIF
076700020305     C     KTAB2         READE     TABEL                                  32
076800020305     C                   ENDDO
076900090113      ***
077000011026     c                   endsr
077100090420     C************************************************************
077200090420     C*  decodifica società
077300090420     C************************************************************
077400090420     C     Decod_societa begSR
077500090420      *
077600090420     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
077700090420     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
077800090420     C                   RESET                   tibsSocI0
077900090420     C                   EVAL      tibsSocI0.idSocieta = wSOCG
078000090420     C
078100090420     C                   CALL      'TIBSSOCR'
078200090420     C                   PARM      'GETANAGRAF'  prmRqsOpCode
078300090420     C                   PARM      *BLANK        prmRpyOpCode
078400090420     C                   PARM      *ZERO         prmRpyIdMsg
078500090420     C                   PARM      'TIBSSOCI0'   prmRqsFormato
078600090420     C                   PARM                    tibsSocI0
078700090420     C                   PARM                    prmRqsDataSize
078800090420     C                   PARM      'TIBSSOCO0'   prmRpyFormato
078900090420     C                   PARM                    tibsSocO0
079000090420     C                   PARM                    prmRpyDataSize
079100090420     c                   if         PRMRPYIDMSG >= 0
079200090420     c                              and TIBSSOCO0.IDSOCIETA <> *blank
079300090420     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
079400090420     c                   else
079500090420     c                   eval      wsocGd = 'Errore'
079600090429     c                   move      'E'           errore
079700090420     c                   end
079800090420      *
079900090420     C                   ENDSR
080000090420     C************************************************************
080100090428     C     test_Pagina   begsr
080200090428      *
080300090428      * attenzione le pagine dispari hanno la casella
080400090428      *     x la posta x la data certa
080500090428     c     cur_page      div       2             pagmeta           5 0
080600090428     c                   mvr                     Pari              3 0
080700090428      *   Se l'attuale pagina è pari e c'è un salto pagina x in testazione
080800090428      *    vado a stampare una dispari con incasellamento data certa
080900090428      * Quindi accendo 66 per andare a stampare una Dispari
081000090428     c     Pari          comp      0                                      66
081100090428      *  Quindi l'accensione del 66 è invertito poichè sarà quello
081200090428      *  che condizionerà la testata futura
081300090428      *
081400090428     C                   ENDSR
081500090428     C************************************************************
081600090427     Oqsysprt   E    66      testata          02
081700090427     o                       costt               33
081800090427     o                       fgtddts           +  2 '  /  /    '
081900091021     o                       novealfa          + 20
082000090504     o                                           94 'FICN14R1'
082100090504     o                                          108 'Pag.'
082200090427     O                       page          z    115
082300090504     O          E    66      testata     1
082400090504     o                                           15 'AUTISTA :'
082500090504     o                       apdPDR        z   +  2
082600090504     o                       apdRSF            +  2
082700090428      *
082800090428     O          E   n66      testata        1 02
082900090424     o                       costt               33
083000090424     o                       fgtddts           +  2 '  /  /    '
083100091021     o                       novealfa          + 20
083200090504     o                                           94 'FICN14R1'
083300090424     o                                          108 'Pag.'
083400090424     O                       page          z    115
083500090504     O          E   n66      testata     1
083600090504     o                                           15 'AUTISTA :'
083700090504     o                       apdPDR        z   +  2
083800090504     o                       apdRSF            +  2
083900020402      *
084000020306     O          E            testa       1
084100020305     o                       separa             100
084200020305     O          E            testa       1
084300020304     o                                           15 'Codice tariffa'
084400090107     o                       fgtctr              19
084500020304     o                       fgttsr              21
084600020313     o                       destsr              38
084700020313     o                                           50 'Progressivo'
084800090113     o                       fgtprg        z     54
084900090113     o                                           62 'Divisa'
085000090113     o                       fgtdiv              66
085100020529     o                       fgtdcv              83
085200020305     O          E            detta       1
085300020402     o                       desctd              26
085400020306     o                                           37 'T.Tariffa'
085500020306     o                       fpttpg              39
085600020306     o                       destpg              55
085700020311     o                                           57 'a'
085800020311     o                       destsm              69
085900020313     o               77                          80 'Tar.fino a'
086000020313     o              n78n77                       78 'Importo'
086100020311     o              n78      fptatas       2     94
086200020307      *arrotondamenti 81 on
086300020306     O          E    81      detta       1
086400020305     o                                           16 'Arrot.limite'
086500020305     o                       fptarls       2     25
086600020305     o                                           33 'Fino a'
086700020305     o                       fptarfs       2     42
086800020305     o                                           50 'Oltre'
086900020305     o                       fptaros       2     59
087000020305     o                                           72 'Rapp.P/Vol'
087100020305     o                       fptrpv        2     77
087200020307      *bonus 82 on
087300020307     O          E    82      detta       1
087400020307     o                                           16 'Tipo pag.Bonus'
087500020307     o                       tpbonus             18
087600020311     o                                         +  2 '(" " S.pag."1"Cons'
087700020311     o                                         +  1 ' "2"Tutte)'
087800020311     o                                         +  3 'Perc.% applic.'
087900020307     o                       pabonus       2   +  1
088000020311     o                                         +  4 'Segno'
088100020311     o                       pmbonus           +  1
088200020307     o                       sabonus       2   +  2
088300020305     O          E            dettab      1
088400020305     o                                           10 'CAP'
088500020305     o                       fpdcap              21
088600020305     o                                           30 'Scaglione'
088700020305     o                       fpdsgls       z     40
088800020311     o              n83                          50 'Importo'
088900020311     o               83                          50 'Perc.% '
089000020305     o                       fpditrs       2     63
089100020305     o                                           72 'Imp.Min'
089200020305     o                       fpdmins       2     85
089300020305     o                                           93 'Imp.Max'
089400020305     o                       fpdmaxs       2    108
089500020311     o                       fpdain             113
089600090113      *-
089700090113     O          E            NO_SuperT   3
089800090113     O                                            1 ' '
089900090113     o                                         +  1 ' CONTATTARE urgentemente'
090000090113     o                                         +  1 ' il  CED.'
090100090113     o                                         +  5 'TARIFFA  NON  stampabile'
090200020313**
090300090428OVRPRTF FILE(QSYSPRT) TOFILE(*LIBL/FICN14P) LPI(6) CPI(15) PAGRTT(0) OUTPTY(2)
090400090504FRONTOVL(*NONE) BACKOVL(*NONE) FORMTYPE('PROVA_TAR')
090500090428OVRPRTF FILE(QSYSPRT) TOFILE(*LIBL/FICN14P) LPI(6) CPI(15) PAGRTT(0) OUTPTY(2)
090600090504FRONTOVL(*NONE) BACKOVL(*NONE) FORMTYPE('PROVA_TAR')
090700090428DLTOVR FILE(QSYSPRT)
