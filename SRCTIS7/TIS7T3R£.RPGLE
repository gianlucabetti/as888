000100051018      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300990915
000400000724     Fazorg01l  if   e           k disk
000500990915     Ftivin00r  uF   E             DISK    usropn
000600051108     FFNVAR00r  O    E             DISK    commit
000700000613     Ftis7t3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900000616     Ftis7t3ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990915
001200080908
001300080908     D TxtInOut        S           2048
001400080908     D ElencoChar      S            256
001500080908     D TipoElenco      S              1
001600080908     D CharSost        S              1
001700080908     D UpperCase       S              1
001800080908     D ChkNull         S              1
001900080908     D CharNull        S              1
002000080908     D EsitoBon        S              1
002100080908
002200000613     D*------------
002300000613     D* COMANDI
002400000613     D*------------
002500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002600000724     D*------------
002700051017     D tibs55ds      e ds                  inz
002800051017     D kpjba         e ds                  inz
002900990915     D psds           sds
003000990915     D  procname         *PROC
003100990921     D tivlrds       e ds                  extname(tivlr00f)
003200990915     D fnvaraar      e ds                  inz
003300990915     D esito           s              1
003400000724     D prmlit          s             10
003500000710     D prmfir          s             10
003600990921     D wrkesito        s                   like(esito)
003700990915     D wrkdata         s               d
003800990915     D wrkora          s               t
003900011123     D depcmd          s            150    INZ(*blanks)
004000990915
004100990915     C                   time                    wrkdata
004200990915     C                   time                    wrkora
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010531     C*
004600010531     C                   exsr      opeini
004700010531     C                   exsr      rwvar
004800010531     C*
004900010531     C                   seton                                        lr
005000010531
005100010531
005200010531
005300010531
005400010531     C*--------------------------------------------------------
005500010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010531     C*--------------------------------------------------------
005700010531     C     PREELA        BEGSR
005800010531     C*
005900010531     C* SE OCCORRE SPEDIRE IN FILIALE
006000010531     C                   if        invfil <> *zeros and
006100010531     C                             flgGiro = '0'
006200010531     C*
006300010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010531     C                   eval      flgGiro = '1'
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800010531     C     invfil        chain     azorg01l
006900000724     C                   if        %found
007000000724     C                   movel(p)  CMD(1)        depcmd
007100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000724     C*
007300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011123     C                   Z-ADD     150           LENGH            15 5
007500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000724     C                   PARM                    depcmd
007700000724     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000616     C*
008100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200000616     C                   movel(p)  CMD(2)        depcmd
008300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008600011123     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000990915     C*
009100000613     C* Stampa riepilogo. (IN SEDE)
009200000616     C                   if        not %open(tis7t3ps)
009300000616     C                   open      tis7t3ps
009400000613     C                   endif
009500010531     C*
009600010531     C                   endif
009700010531     C*
009800010531     C                   ENDSR
009900010531     C***
010000010531
010100010531
010200010531
010300010531
010400010531
010500010531
010600010531
010700010531
010800010531
010900010531     C*--------------------------------------------------------
011000010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010531     C*--------------------------------------------------------
011200010531     C     ENDELA        BEGSR
011300010531     C*
011400010531     C* Stampa riepilogo. (IN FILIALE)
011500010531     C                   if        not %open(tis7t3p)
011600010531     C                   open      tis7t3p
011700010531     C                   endif
011800010531     C*
011900030407     C   50              except    riepilogo
012000010531     C*
012100010531     C                   if        %open(tis7t3p)
012200010531     C                   close     tis7t3p
012300010531     C                   endif
012400000613     C*
012500000616     C                   if        %open(tis7t3ps)
012600030407     C   50              except    riepsto
012700000616     C                   close     tis7t3ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011123     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010531     C                   ENDSR
013700010531     C***
013800010531
013900010531
014000010531
014100010531
014200990915
014300910830     C*--------------------------------------------------------
014400941229     C* RWVAR   LEGGE FNVARAAR E SCRIVE FNVAR00R              *
014500910830     C*--------------------------------------------------------
014600941229     C     RWVAR         BEGSR
014700990915     C*
014800990915     C                   if        not %open(tivin00r)
014900990915     C                   open      tivin00r
015000990915     C                   endif
015100990915     C*
015200010601     C                   MOVE      *ZERO         §CTROK
015300010601     C                   MOVE      *ZERO         §CTRNO
015400921023     C                   DO        *HIVAL
015500010618     C*
015600990915     C                   READ      tivin00r                               70
015700030305     C*
015800030305     C* Dopo ogni lettura azzero gli indicatori di errore
015900030305     C                   SETOFF                                       31
016000010618     C*
016100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016200010618     C                   if        vinflg = '1'
016300010618     C                   eval      flgOk = '1'
016400010618     C                   endif
016500010618     C*
016600000905     C                   if        vindta > *blanks
016700990915     C*
016800010601     C                   if        *in70 = *off and
016900990915     C                             (vinflg = *blanks
017000010711     C                              or vinflg = '0'
017100010711     C                              or vinflg = '2')
017200990915     C*
017300010531     C                   clear                   FNVAR000
017400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017500010711     C                   if        vinflg = *blanks or vinflg = '0'
017600010711     C                   clear                   vinmsg
017700010711     C                   endif
017800010711     C*
017900990915     C                   eval      fnvaraar = vindta
018000991022     C*
018100991022     C                   movel     vindta        wrkuno            1
018200991022     C                   testb     x'3F'         wrkuno                   02
018300991022     C                   if        not *in02
018400010531     C*
018500010531     C                   TESTN                   VHRLNP               30
018600010531     C  N30              TESTN                   VHRLNP                 30
018700010531     C  N30              TESTN                   VHRLNP                   30
018800010531     C                   MOVE      VHRLNP        W001A             1
018900010531     C     *IN30         IFEQ      *OFF
019000010531     C     W001A         ORLT      '0'
019100010531     C                   SETON                                        31
019200010531     C                   eval      vinmsg = %trimr(vinmsg)
019300010531     C                             + ' ' + 'VHRLNP'
019400010531     C                   ELSE
019500010531     C                   MOVE      VHRLNP        VARLNP
019600010531     C     VARLNP        IFLE      *ZEROS
019700010531     C                   SETON                                        31
019800010531     C                   eval      vinmsg = %trimr(vinmsg)
019900010531     C                             + ' ' + 'VHRLNP'
020000010531     C                   ENDIF
020100010531     C                   ENDIF
020200010531     C*
020300010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020400010531     C                   eval      depfil = VARLNP
020500010531     C                   exsr      repfil
020600010601     C                   if        depfil = invfil
020700010531     C*
020800010531     C                   exsr      PREELA
020900010531     C*
021000010531     C                   TESTN                   VHRCCM               30
021100010531     C  N30              TESTN                   VHRCCM                 30
021200010531     C  N30              TESTN                   VHRCCM                   30
021300010531     C                   MOVE      VHRCCM        W001A             1
021400010531     C     *IN30         IFEQ      *OFF
021500010531     C     W001A         ORLT      '0'
021600010531     C                   SETON                                        31
021700010604     C                   eval      vinmsg = %trimr(vinmsg)
021800010604     C                             + ' ' + 'VHRCCM'
021900010531     C                   ELSE
022000010531     C                   MOVE      VHRCCM        VARCCM
022100010531     C     VARCCM        IFLE      *ZEROS
022200010531     C                   SETON                                        31
022300010604     C                   eval      vinmsg = %trimr(vinmsg)
022400010604     C                             + ' ' + 'VHRCCM'
022500010531     C                   ENDIF
022600010531     C                   ENDIF
022700010531     C*
022800130108     C                   MOVEL     datcor        VARAAS
022900010531     C                   TESTN                   VHRAAS               30
023000010531     C  N30              TESTN                   VHRAAS                 30
023100010531     C  N30              TESTN                   VHRAAS                   30
023200010531     C                   MOVE      VHRAAS        W001A             1
023300010531     C     *IN30         IFEQ      *OFF
023400010531     C     W001A         ORLT      '0'
023500010531     C                   SETON                                        31
023600010531     C                   eval      vinmsg = %trimr(vinmsg)
023700010531     C                             + ' ' + 'VHRAAS'
023800010531     C                   ELSE
023900010531     C                   MOVE      VHRAAS        VARAAS
024000010531     C     VARAAS        IFLE      *ZEROS
024100010531     C                   SETON                                        31
024200010604     C                   eval      vinmsg = %trimr(vinmsg)
024300010604     C                             + ' ' + 'VHRAAS'
024400010531     C                   ENDIF
024500010531     C                   ENDIF
024600010531     C*
024700010531     C                   TESTN                   VHRNRS               30
024800010531     C  N30              TESTN                   VHRNRS                 30
024900010531     C  N30              TESTN                   VHRNRS                   30
025000010531     C                   MOVE      VHRNRS        W001A             1
025100010531     C     *IN30         IFEQ      *OFF
025200010531     C     W001A         ORLT      '0'
025300010531     C     W001A         ANDNE     *BLANK
025400010531     C                   SETON                                        31
025500010531     C                   eval      vinmsg = %trimr(vinmsg)
025600010531     C                             + ' ' + 'VHRNRS'
025700010531     C                   ELSE
025800010531     C                   MOVE      VHRNRS        VARNRS
025900010531     C     VARNRS        IFLT      *ZEROS
026000010531     C                   SETON                                        31
026100010604     C                   eval      vinmsg = %trimr(vinmsg)
026200010604     C                             + ' ' + 'VHRNRS'
026300010531     C                   ENDIF
026400010531     C                   ENDIF
026500010531     C*
026600010531     C                   TESTN                   VHRNSP               30
026700010531     C  N30              TESTN                   VHRNSP                 30
026800010531     C  N30              TESTN                   VHRNSP                   30
026900010531     C                   MOVE      VHRNSP        W001A             1
027000010531     C     *IN30         IFEQ      *OFF
027100010531     C     W001A         ORLT      '0'
027200010531     C                   SETON                                        31
027300010531     C                   eval      vinmsg = %trimr(vinmsg)
027400010531     C                             + ' ' + 'VHRNSP'
027500010531     C                   ELSE
027600010531     C                   MOVE      VHRNSP        VARNSP
027700010531     C     VARNSP        IFLE      *ZEROS
027800010531     C                   SETON                                        31
027900010604     C                   eval      vinmsg = %trimr(vinmsg)
028000010604     C                             + ' ' + 'VHRNSP'
028100010531     C                   ENDIF
028200010531     C                   ENDIF
028300010531     C*
028400130108     C                   MOVEL     datcor        VARAGC
028500010531     C                   TESTN                   VHRAGC               30
028600010531     C  N30              TESTN                   VHRAGC                 30
028700010531     C  N30              TESTN                   VHRAGC                   30
028800010531     C                   MOVE      VHRAGC        W001A             1
028900010531     C     *IN30         IFEQ      *OFF
029000010531     C     W001A         ORLT      '0'
029100010531     C                   SETON                                        31
029200010531     C                   eval      vinmsg = %trimr(vinmsg)
029300010531     C                             + ' ' + 'VHRAGC'
029400010531     C                   ELSE
029500010531     C                   MOVE      VHRAGC        VARAGC
029600010531     C     VARAGC        IFLE      *ZEROS
029700010531     C                   SETON                                        31
029800010604     C                   eval      vinmsg = %trimr(vinmsg)
029900010604     C                             + ' ' + 'VHRAGC'
030000010531     C                   ENDIF
030100010531     C                   ENDIF
030200010531     C*
030300010531     C                   TESTN                   VHRFGC               30
030400010531     C  N30              TESTN                   VHRFGC                 30
030500010531     C  N30              TESTN                   VHRFGC                   30
030600010531     C                   MOVE      VHRFGC        W001A             1
030700010531     C     *IN30         IFEQ      *OFF
030800010531     C     W001A         ORLT      '0'
030900010531     C                   SETON                                        31
031000010531     C                   eval      vinmsg = %trimr(vinmsg)
031100010531     C                             + ' ' + 'VHRFGC'
031200010531     C                   ELSE
031300010531     C                   MOVE      VHRFGC        VARFGC
031400010531     C     VARFGC        IFLE      *ZEROS
031500010531     C                   SETON                                        31
031600010531     C                   eval      vinmsg = %trimr(vinmsg)
031700010531     C                             + ' ' + 'VHRFGC'
031800010531     C                   ENDIF
031900010531     C                   ENDIF
032000010531     C*
032100010531     C                   TESTN                   VHRNGC               30
032200010531     C  N30              TESTN                   VHRNGC                 30
032300010531     C  N30              TESTN                   VHRNGC                   30
032400010531     C                   MOVE      VHRNGC        W001A             1
032500010531     C     *IN30         IFEQ      *OFF
032600010531     C     W001A         ORLT      '0'
032700010531     C                   SETON                                        31
032800010531     C                   eval      vinmsg = %trimr(vinmsg)
032900010531     C                             + ' ' + 'VHRNGC'
033000010531     C                   ELSE
033100010531     C                   MOVE      VHRNGC        VARNGC
033200010531     C     VARNGC        IFLE      *ZEROS
033300010531     C                   SETON                                        31
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHRNGC'
033600010531     C                   ENDIF
033700010531     C                   ENDIF
033800010531     C*
033900010531     C                   TESTN                   VHRNPR               30
034000010531     C  N30              TESTN                   VHRNPR                 30
034100010531     C  N30              TESTN                   VHRNPR                   30
034200010531     C                   MOVE      VHRNPR        W001A             1
034300010531     C     *IN30         IFEQ      *OFF
034400010531     C     W001A         ORLT      '0'
034500010531     C                   SETON                                        31
034600010531     C                   eval      vinmsg = %trimr(vinmsg)
034700010531     C                             + ' ' + 'VHRNPR'
034800010531     C                   ELSE
034900010531     C                   MOVE      VHRNPR        VARNPR
035000010531     C     VARNPR        IFLT      *ZEROS
035100010531     C                   SETON                                        31
035200010604     C                   eval      vinmsg = %trimr(vinmsg)
035300010604     C                             + ' ' + 'VHRNPR'
035400010531     C                   ENDIF
035500010531     C                   ENDIF
035600010531     C*
035700941229     C                   MOVE      VHRDMC        VARDMC
035800010531     C*
035900010531     C                   TESTN                   VHRDTR               30
036000010531     C  N30              TESTN                   VHRDTR                 30
036100010531     C  N30              TESTN                   VHRDTR                   30
036200010531     C                   MOVE      VHRDTR        W001A             1
036300010531     C     *IN30         IFEQ      *OFF
036400010531     C     W001A         ORLT      '0'
036500010531     C                   SETON                                        31
036600010531     C                   eval      vinmsg = %trimr(vinmsg)
036700010531     C                             + ' ' + 'VHRDTR'
036800010531     C                   ELSE
036900010531     C                   MOVE      VHRDTR        VARDTR
037000010531     C     VARDTR        IFLT      *ZEROS
037100010531     C                   SETON                                        31
037200010604     C                   eval      vinmsg = %trimr(vinmsg)
037300010604     C                             + ' ' + 'VHRDTR'
037400010531     C                   ENDIF
037500010531     C                   ENDIF
037600010531     C*
037700010531     C                   TESTN                   VHRDRD               30
037800010531     C  N30              TESTN                   VHRDRD                 30
037900010531     C  N30              TESTN                   VHRDRD                   30
038000010531     C                   MOVE      VHRDRD        W001A             1
038100010531     C     *IN30         IFEQ      *OFF
038200010531     C     W001A         ORLT      '0'
038300010531     C                   SETON                                        31
038400010531     C                   eval      vinmsg = %trimr(vinmsg)
038500010531     C                             + ' ' + 'VHRDRD'
038600010531     C                   ELSE
038700010531     C                   MOVE      VHRDRD        VARDRD
038800010531     C     VARDRD        IFLT      *ZEROS
038900010531     C                   SETON                                        31
039000010604     C                   eval      vinmsg = %trimr(vinmsg)
039100010604     C                             + ' ' + 'VHRDRD'
039200010531     C                   ENDIF
039300010531     C                   ENDIF
039400080908     C*
039500080908     C* "bonifico" i dati di input da eventuali caratteri indesiderati
039600080908     C                   exsr      exeBon
039700010601     C*
039800010601     C  N31              ADD       1             §CTROK            7 0
039900010601     C   31              ADD       1             §CTRNO            7 0
040000010601     C  N31              WRITE     FNVAR000
040100010601     C*
040200010601     C                   if        *in31 = *off
040300010601     C                   eval      vinflg = '1'
040400010601     C                   else
040500010601     C                   eval      vinflg = '2'
040600010601     C                   endif
040700010601     C                   endif
040800010601     C                   endif
040900010604     C*
041000010601     C                   endif
041100990915     C*
041200000905     C                   else
041300000905     C                   eval      vinflg = '1'
041400990915     C                   endif
041500000905     C*
041600000905     C  N70              update    tivin000
041700991022     C*
041800991022     C  N70              ENDdo
041900990915     C*
042000010531     C*
042100010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
042200010531     C                   if        cntNonEl = *zeros or
042300010531     C                             flgMulti = '0'
042400010601     C* Se non ci sono record con errori ...
042500010601     C                   if        §ctrno = 0 and
042600010601     C                             flgStato <> '2'
042700010601     C* ... restituisco esito OK.
042800010601     C                   eval      wrkesito = '0'
042900010601     C                   else
043000010601     C                   if        §ctrok > 0
043100010601     C                   eval      wrkesito = '1'
043200010601     C                   else
043300010615     C                   if        flgOk = '0'
043400010615     C                   eval      wrkesito = '2'
043500010615     C                   else
043600010615     C                   eval      wrkesito = '6'
043700010615     C                   endif
043800010601     C                   endif
043900010601     C                   endif
044000010531     C                   else
044100010531     C                   eval      wrkesito = '9'
044200010531     C                   endif
044300990915     C*
044400990915     C                   if        %open(tivin00r)
044500990915     C                   close     tivin00r
044600990915     C                   endif
044700010531     C*
044800010531     C                   if        vlrpoi <> 999
044900010531     C                   eval      invfil = vlrpoi
045000010531     C                   endif
045100990921     C*
045200010531     C                   if        invfil > *zeros
045300051108     C                   COMMIT
045400990921     C                   exsr      invio
045500000710     C                   endif
045600010612     C*
045700010612     C                   if        flgGiro = '1'
045800010612     C                   exsr      endela
045900010612     C                   endif
046000010531     C*
046100010531     C                   ENDSR
046200010531     C***
046300010531
046400010531
046500010531
046600010531
046700010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
046800010531     C     repfil        BEGSR
046900010531     C*
047000010531     C                   if        invfil = *zeros and
047100010531     C                             depfil > *zeros and
047200010613     C                             (vinflg = *blanks or
047300010613     C                              vinflg = *zeros)
047400010531     C
047500010531     C                   eval      invfil = depfil
047600010531     C                   endif
047700010531     C*
047800010531     C                   if        depfil <> invfil and
047900010531     C                             invfil > *zeros
048000010531     C                   eval      flgMulti = '1'
048100010531     C                   if        vinflg = *blanks
048200010531     C                   add       1             cntNonEl
048300010531     C                   endif
048400010531     C                   endif
048500010531     C*
048600010531     C                   if        vinflg = '2'
048700010531     C                   eval      flgStato = '2'
048800010531     C                   endif
048900010531     C*
049000010531     C                   ENDSR
049100010531     C***
049200010531
049300010531
049400010531
049500010531
049600990921
049700990921      /TITLE Invio dei dati al punto operativo.
049800990921     C     invio         begsr
049900990921     C*
050000051017     C                   clear                   tibs55ds
050100051017     C                   move      invfil        i50apo
050200051017     C                   call(e)   'TIBS55R'
050300051017     C                   parm                    tibs55ds
050400051017     C*
050500051018     C                   if        %error or
050600051017     C                             o50err <> *blanks
050700051017     C                   eval      wrkesito = '3'
050800051017     C                   else
050900051018     C*
051000051018     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
051100051018     C                   clear                   KPJBA
051200051018     C                   movel     'LG95'        KCOAZ
051300051018     C                   movel     o50asi        KNSIF
051400051018     C*
051500051018     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
051600051018     C                   movel     'EDPCEDXXX'   KNMUS
051700051018     C                   z-add     *zeros        KNRAZ
051800051018     C                   movel     'DSP01'       KNMTD
051900051018     C                   z-add     *zeros        KNRVJ
052000051018     C                   z-add     *zeros        KNRJO
052100051018     C                   movel     'N'           KSTJO
052200051018     C                   z-add     *zeros        KTMIN
052300051018     C                   movel     'B'           KTPAZ
052400051018     C                   movel     'RICEZ_VAR'   KNMEB
052500051018     C                   z-add     *zeros        KNMSB
052600051018     C                   z-add     *zeros        KKEYF
052700051018     C                   z-add     *zeros        KRCDS
052800051018     C                   movel     'J'           KEXCN
052900051018     C                   movel     'P5'          KCOJB
053000051018     C***                movel     'KEDI'        KQIEB
053100051018     C***                movel     'KEDI'        KQEBI
053200051018     C                   movel     'N'           KCANC
053300051108     C***                call      'BCH10'
053400051108     C***                parm                    KPJBA
053500051108     C                   call      'FNLG95C2'
053600051108     C                   parm                    KPJBA
053700051017     C                   endif
053800990921     C*
053900990921     C                   endsr
054000010531
054100010531
054200010531
054300010531
054400010531      /TITLE Invio dei dati al punto operativo.
054500010531     C     opeini        BEGSR
054600010531     C*
054700010531     C* Inizializzo flag e contatori operativi
054800010531     C                   movel     '0'           flgGiro           1
054900010531     C                   movel     '0'           flgMulti          1
055000010531     C                   movel     '1'           flgStato          1
055100010615     C                   movel     '0'           flgOk             1
055200010531     C                   z-add     *zeros        cntNonEl         10 0
055300010531     C                   z-add     *zeros        depfil            3 0
055400010531     C                   z-add     *zeros        invfil            3 0
055500030407     C*
055600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
055700040311     C                   if        %subst(vlrppt:1:1) = 'S'
055800040311     C                   seton                                        50
055900030407     C                   else
056000040311     C                   setoff                                       50
056100030407     C                   endif
056200010531     C*
056300010531     C                   ENDSR
056400010531     C***
056500051108
056600051108
056700051108
056800051108      /TITLE Gestione errore generico
056900051108     C     *pssr         BEGSR
057000051108     C*
057100051108     C* Se errore => emetto operazione d ROLLBACK
057200051108     C                   ROLBK
057300051108     C*
057400051108     C                   ENDSR     '*CANCL'
057500080908     C***
057600010531
057700080908
057800080908
057900080908      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
058000080908     C     exeBon        BEGSR
058100080908     C*
058200080908     C* Imposto i parametri x il *pgm d "bonifica"
058300080908     C                   EVAL      ElencoChar = *blanks
058400080908     C                   EVAL      TipoElenco = *blanks
058500080908     C                   EVAL      CharSost   = *blanks
058600080908     C                   EVAL      UpperCase  = '1'
058700080908     C                   EVAL      ChkNull    = '1'
058800080908     C                   EVAL      CharNull   = *blanks
058900080908     C*
059000080908     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VARDMC
059100080908     C                   EVAL      TxtInOut   = VARDMC
059200080908     C                   EVAL      EsitoBon   = *blanks
059300080908     C*
059400080908     C                   CALL      'XCHKCHAR'
059500080908     C                   PARM                    TxtInOut
059600080908     C                   PARM                    ElencoChar
059700080908     C                   PARM                    TipoElenco
059800080908     C                   PARM                    CharSost
059900080908     C                   PARM                    UpperCase
060000080908     C                   PARM                    ChkNull
060100080908     C                   PARM                    CharNull
060200080908     C                   PARM                    EsitoBon
060300080908     C*
060400080908     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
060500080908     C                   IF        EsitoBon = '1'
060600080908     C                   EVAL      VARDMC = TxtInOut
060700080908     C                   ENDIF
060800080908     C*
060900080908     C                   ENDSR
061000010531
061100080908
061200990915
061300000613     C     *inzsr        BEGSR
061400990915     C*
061500990915     C     *entry        plist
061600990921     C                   parm                    tivlrds
061700990921     C                   parm      wrkesito      esito
061800000724     C                   parm                    prmlit
061900000710     C                   parm                    prmfir
062000000613     C*
062100130108     C                   z-add     *zeros        datcor            8 0
062200130108     C                   eval      datcor = %dec(%date() : *ISO)
062300130108     C*
062400000613     C                   ENDSR
062500000613     C***
062600990915
062700000613     Otis7t3p   E            riepilogo         2
062800990915     O                                              'Upload via Internet'
062900990915     O                                           +1 'Traduzione TIVIN00R -'
063000051018     O                                           +1 'FNVAR00R'
063100990915     O                                           +1 'Disposizioni giacenza'
063200990915     O          E            riepilogo   2
063300990915     O                       wrkdata
063400990915     O                       wrkora              +1
063500990915     O                       procname            +1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Cliente..................:'
063800990915     O                       VARCCM        z     +1
063900990915     O          E            riepilogo   2
064000990921     O                                              'Riferimento Strategi.....:'
064100990921     O                       vlrhdl              +1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Giusti...................:'
064400010601     O                       §CTROK        2   +  1
064500000613
064600000616     Otis7t3ps  E            riepsto           2
064700000613     O                                              'Upload via Internet'
064800000613     O                                           +1 'Traduzione TIVIN00R -'
064900051018     O                                           +1 'FNVAR00R'
065000000613     O                                           +1 'Disposizioni giacenza'
065100000616     O          E            riepsto     2
065200000613     O                       wrkdata
065300000613     O                       wrkora              +1
065400000613     O                       procname            +1
065500000616     O          E            riepsto     2
065600000613     O                                              'Cliente..................:'
065700000613     O                       VARCCM        z     +1
065800000616     O          E            riepsto     2
065900000613     O                                              'Riferimento Strategi.....:'
066000000613     O                       vlrhdl              +1
066100000616     O          E            riepsto     2
066200000613     O                                              'Giusti...................:'
066300010601     O                       §CTROK        2   +  1
066400000613** CMD - COMANDI CL
066500040310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T3P)  SAVE(*YES) FORMTYPE(RICCLI) OUTQ(
066600040310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T3PS) SAVE(*YES) FORMTYPE(RICCLI) OUTQ(
066700000616DLTOVR FILE(TIS7T3P TIS7T3PS) LVL(*)
066800000613
066900000613
