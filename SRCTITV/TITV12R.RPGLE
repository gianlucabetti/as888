000100020507      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente FED-EX Cod. 0891495)
000200990908     H dftactgrp(*yes)
000300990908
000400030624     Ffnacr01l  if   e           k disk
000500090618     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700010122     FFNVAOwwr  O    E             DISK    usropn
000800011105     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000990908
001100030624     D*------------------
001200030624     D* DS REPERIMENTO DATI CLIENTE
001300030624     D*-------------------
001400030624     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001500030624     D ACODS         E DS                  EXTNAME(CNACO00F)
001600030624     D INDDS         E DS                  EXTNAME(CNIND00F)
001700030624     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001800030624     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300010122     D*
002400050826     D fnvapds       e ds                  extname(fnvap00f)
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600011105     D dorm01        e ds
002700011119     D dfar          e ds
002800020507     D tnsym5cds     e ds
002900090618     d trul33ds      e ds
003000090618     d kpjba         e ds
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600020507     D wrkRFA          s                   LIKE(vaorfa)
003700090618     d §numori         s              7  0 inz
003800011105     D*-------------------
003900011105     D* DS "XSRDA8" - controllo data (8)
004000011105     D*-------------------
004100011105     D wlbda8          DS                  INZ
004200011105     D  g08dat                 1      8  0
004300011105     D  g08inv                 9     16  0
004400011105     D  g08err                17     17
004500011105     D  g08tgi                18     22  0
004600020507     D*-------------------
004700020507     D* DS x ridefinizione campi note
004800020507     D*-------------------
004900020507     D DSWNOTE         DS                  INZ
005000020508     D  WNote1_1               1     35
005100020508     D  WNote1_2              36     70
005200020508     D  WNote1                 1     70
005300020508     D  WNote2                71    140
005400020508     D  WNote3               141    210
005500020508     D  WNote4               211    280
005600020508     D  WNote5               281    350
005700011122     D*-------------------
005800010731     D* COSTANTI
005900011122     D*-------------------
006000010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006100010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006200990908
006300091026
006400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006600091026
006700010528
006800010528
006900010528
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300000724     C*
007400010528     C                   exsr      opeini
007500010528     C                   exsr      rwvao
007600010528     C*
007700010528     C                   seton                                        lr
007800010528
007900010528
008000010528
008100010528
008200010528     C*--------------------------------------------------------
008300010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010528     C*--------------------------------------------------------
008500010528     C     PREELA        BEGSR
008600010528     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800010528     C                   if        invfil <> *zeros and
008900010528     C                             flgGiro = '0'
009000010528     C*
009100010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010528     C                   eval      flgGiro = '1'
009300010529     C*
009400010529     C                   endif
009500010528     C*
009600010528     C                   ENDSR
009700010528     C***
009800010528
009900010528
010000010528     C*--------------------------------------------------------
010100010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010528     C*--------------------------------------------------------
010300010528     C     ENDELA        BEGSR
010400000613     C*
010500000613     C*
010600010528     C                   ENDSR
010700010528     C***
010800990908
010900010528
011000010528
011100910830     C*--------------------------------------------------------
011200020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
011300910830     C*--------------------------------------------------------
011400010122     C     RWVAO         BEGSR
011500010528     C*
011600990914     C                   if        not %open(tivin00r)
011700990908     C                   open      tivin00r
011800990914     C                   endif
011900010122     C                   if        not %open(fnvaowwr)
012000010122     C                   open      fnvaowwr
012100990914     C                   endif
012200990910     C*
012300990910     C                   clear                   §CTROK
012400990910     C                   clear                   §CTRMO
012500990910     C                   clear                   §CTRNO
012600990910     C*
012700921023     C                   DO        *HIVAL
012800990913     C*
012900990915     C                   READ      tivin00r                               70
013000010731     C*
013100010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
013200010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
013300010618     C*
013400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013500010618     C                   if        vinflg = '1'
013600010618     C                   eval      flgOk = '1'
013700010618     C                   endif
013800010618     C*
013900000905     C                   if        vindta > *blanks
014000000613     C                   add       1             rrnum
014100990913     C*
014200010711     C                   if        *in70 = *off and
014300010711     C                             (vinflg = *blanks
014400010711     C                              or vinflg = '0'
014500010711     C                              or vinflg = '2')
014600990913     C*
014700010528     C                   clear                   FNVAO000
014800011105     C                   clear                   TIORI000
014900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015000010711     C                   if        vinflg = *blanks or vinflg = '0'
015100010711     C                   clear                   vinmsg
015200010711     C                   endif
015300020507     C*
015400020507     C* Testo il tipo record x determinare le operazioni da eseguire
015500020507     C                   exsr      inzvar
015600020507     C                   exsr      defcam
015700020507     C                   if        %subst(vindta:1:3) = 'PUR'
015800020507     C                   setoff                                       55
015900020507     C                   exsr      impvao
016000020507     C                   endif
016100020507     C                   if        %subst(vindta:1:3) = 'ROC'
016200020508     C                   setoff                                       55
016300020508     C                   exsr      impvao
016400020507     C                   endif
016500020507     C                   if        %subst(vindta:1:3) = 'PUM'
016600020507     C                   seton                                        55
016700020507     C                   eval      wrkRFA = ((%subst(vindta:28:1))  +
016800020508     C                                       (%subst(vindta:41:6))   +
016900020508     C                                       (%subst(vindta:47:7)))
017000020507     C                   eval      DSWNOTE = %trim(%subst(vindta:54:350))
017100020507     C
017200020507     C
017300020507     C                   eval      YM5TES = 'ORM FEDEX (0891495) ' +
017400020507     C                                      'RIF:' + wrkRFA
017500020508     C                   eval      YM5RIG = 8
017600020507     C                   eval      YM5T01 = '*** RETTIFICA ORM ***'
017700020508     C                   eval      YM5T02 = 'DATA RETTIFICA: ' +
017800020508     C                                      %subst(vindta:4:4) + '/' +
017900020508     C                                      %subst(vindta:8:2) + '/' +
018000020508     C                                      %subst(vindta:10:2)
018100020508     C                   eval      YM5T03 = 'ORA  RETTIFICA: ' +
018200020508     C                                      %subst(vindta:12:2) + ':' +
018300020508     C                                      %subst(vindta:14:2)
018400020508     C                   eval      YM5T04 = WNote1
018500020508     C                   eval      YM5T05 = WNote2
018600020508     C                   eval      YM5T06 = WNote3
018700020508     C                   eval      YM5T07 = WNote4
018800020508     C                   eval      YM5T08 = WNote5
018900020507     C                   eval      YM5FDE = VAOPOE
019000020507     C                   eval      YM5UDE = 'FED'
019100020507     C                   exsr      sndptAS
019200020507     C                   endif
019300010122     C*
019400010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019500010528     C                   eval      depfil = VAOPOE
019600010528     C                   exsr      repfil
019700020507     C                   if        depfil = invfil
019800010528     C*
019900010528     C                   exsr      PREELA
020000010710     C*
020100930409     C*
020200010604     C  N31              ADD       1             §CTROK            7 0
020300010604     C   32              ADD       1             §CTRMO            7 0
020400010604     C   31              ADD       1             §CTRNO            7 0
020500020507     C*
020600020507     C                   IF        *IN55 = *OFF
020700030624     C  N31              EXSR      CHKCOR
020800011105     C  N31              EXSR      WRIORI
020900050826     C  N31              EXSR      WRIVGD
021000010122     C  N31              WRITE     FNVAO000
021100020507     C                   ENDIF
021200990910     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500990910     C                   eval      vinflg = '1'
021600990910     C                   else
021700990910     C                   eval      vinflg = '2'
021800991022     C                   endif
021900990910     C                   endif
022000010604     C*
022100010604     C                   endif
022200000905     C*
022300000905     C                   else
022400000905     C                   eval      vinflg = '1'
022500000905     C                   endif
022600000905     C*
022700000905     C  N70              update    tivin000
022800991022     C*
022900991022     C  N70              ENDdo
023000010528     C*
023100010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023200010528     C                   if        cntNonEl = *zeros or
023300010528     C                             flgMulti = '0'
023400010528     C* Se non ci sono record con errori ...
023500010528     C                   if        §ctrno = 0 and
023600010604     C                             §ctrmo = 0 and
023700010528     C                             flgStato <> '2'
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100000710     C                   if        §ctrok > 0
024200990921     C                   eval      wrkesito = '1'
024300000710     C                   else
024400010615     C                   if        flgOk = '0'
024500010615     C                   eval      wrkesito = '2'
024600010615     C                   else
024700010615     C                   eval      wrkesito = '6'
024800010615     C                   endif
024900990910     C                   endif
025000010528     C                   endif
025100010529     C                   else
025200010529     C                   eval      wrkesito = '9'
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800010122     C                   if        %open(fnvaowwr)
025900010122     C                   close     fnvaowwr
026000990914     C                   endif
026100990910     C*
026200010528     C                   if        vlrpoi <> 999
026300010528     C                   eval      invfil = vlrpoi
026400010528     C                   endif
026500010528     C*
026600990920     C                   if        §ctrok > 0
026700010528     C                             and invfil > *zeros
026800000613     C                   exsr      invio
026900990920     C                   endif
027000010612     C*
027100010612     C                   if        flgGiro = '1'
027200010612     C                   exsr      endela
027300010612     C                   endif
027400010528     C*
027500910830     C                   ENDSR
027600000613     C***
027700990920
027800020507
027900020507     C*----------------------------------------------------*
028000020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028100020507     C*----------------------------------------------------*
028200020507     C     INZVAR        BEGSR
028300020507     C*
028400020507     C                   Z-ADD     *zeros        Num5_0            5 0
028500020507     C                   Z-ADD     *zeros        Num3_0            3 0
028600020507     C                   CLEAR                   DSWNOTE
028700020507     C                   CLEAR                   TNSYM5CDS
028800020507     C*
028900020507     C                   ENDSR
029000020507     C*----------------------------------------------------*
029100020507     C*  IMPOSTAZIONE CAMPI COSTANTI
029200020507     C*----------------------------------------------------*
029300020507     C     DEFCAM        BEGSR
029400020507     C*
029500020507     C* Imposto i valori di default...
029600020507     C                   EVAL      VAOPOE = 089
029700020507     C                   EVAL      VAOTOR = 'S'
029800020507     C                   EVAL      VAOTCO = 'F'
029900020507     C                   EVAL      VAOCOR = 0891495000
030000020620     C                   EVAL      VAOCRC = 0891495100
030100020507     C                   EVAL      VAOFFD = *blanks
030200020507     C                   EVAL      VAOPAG = 'D'
030300020507     C                   EVAL      VAOKSC = 0891495
030400020507     C                   EVAL      VAODDT = 'S'
030500020507     C                   EVAL      VAOCTR = '000'
030600040713     C                   EVAL      VAOPOC = 137
030700020507     C* ... e poi verifico se sono stati passati come parametri
030800020507     C                   IF        vlrppt > *blanks
030900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
031000020507     C                   EXSR      CHKNUM
031100020507     C                   IF        PiInt=*on
031200020507     C                   Z-ADD     PiVal         VAOCOR
031300020507     C                   ENDIF
031400020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031500020507     C                   EXSR      CHKNUM
031600020507     C                   IF        PiInt=*on
031700020507     C                   Z-ADD     PiVal         VAOPOE
031800020507     C                   ENDIF
031900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
032000020507     C                   EXSR      CHKNUM
032100020507     C                   IF        PiInt=*on
032200020507     C                   Z-ADD     PiVal         Num3_0
032300020507     C                   MOVEL     NUM3_0        VAOCTR
032400020507     C                   ENDIF
032500040713     C                   EVAL      PiStr=%trim(%subst(vlrppt:17:3))
032600040713     C                   EXSR      CHKNUM
032700040713     C                   IF        PiInt=*on
032800040713     C                   Z-ADD     PiVal         VAOPOC
032900040713     C                   ENDIF
033000020507     C                   ENDIF
033100020507     C*
033200020507     C                   ENDSR
033300020507     C*----------------------------------------------------*
033400020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
033500020507     C*----------------------------------------------------*
033600020507     C     IMPVAO        BEGSR
033700020507     C*
033800020507     C                   SETOFF                                       3132
033900020507     C*
034000020507     C* Reperimento campi ALFA
034100020507     C*
034200020507     C* Considerazioni sulla ragione sociale del destinatario da indicare
034300020507     C                   EVAL      VAORSR=%trim(%subst(vindta:41:35))
034400020507     C* == verifico se esiste il carattere @e, se c'è diventa A
034500020507     C     '@':'A'       XLATE     VAORSR        VAORSR
034600020507     C* ==
034700020507     C                   EVAL      VAOINR=%trim(%subst(vindta:76:35))
034800020507     C                   EVAL      VAOLOR=%trim(%subst(vindta:111:25))
034900020507     C                   EVAL      VAOPRR=%trim(%subst(vindta:145:2))
035000020507     C                   EVAL      VAORER=%trim(%subst(vindta:162:20))
035100020507     C                   EVAL      VAOTER=%trim(%subst(vindta:147:15))
035200020507     C
035300020507     C                   EVAL      VAORFA=((%subst(vindta:1:1))   +
035400020507     C                                     (%subst(vindta:6:6))   +
035500020507     C                                     (%subst(vindta:24:7)))
035600020508     C*
035700020508     C* Se trattasi di ROC evidenzio nel campo "Descrizione natura merce" che la merce
035800020508     C* viene portata a magazzino dal cliente.
035900020508     C                   IF        %subst(vindta:1:3)='ROC'
036000020508     C                   EVAL      VAONAM='MERCE PORTATA A MAGAZZ. DAL CLIENTE'
036100020508     C                   ENDIF
036200020507     C*
036300020507     C* Costruisco i campi note
036400020507     C                   EVAL      DSWNOTE = %trim(%subst(vindta:387:350))
036500020508     C                   EVAL      VAONO1 = WNote1_1
036600020508     C                   EVAL      VAONO2 = WNote1_2
036700020507     C* Se ci sono + di 70 caratteri di note, quelle che non riesco a inserire nel file FNVAO
036800020507     C* le mando tramite messaggio AS al FED della filiale di emissione dell'ORM
036900020507     C                   IF        %len(%trim(DSWNOTE)) > 70
037000020507     C                   EVAL      YM5TES = 'ORM FEDEX (0891495) ' +
037100020507     C                                      'RIF:' + VAORFA
037200020508     C                   EVAL      YM5RIG = 6
037300020508     C                   EVAL      YM5T01 = '*** NOTE NON COMPLETAMENTE ' +
037400020508     C                                      'INSERITE IN ORM ***'
037500020508     C                   EVAL      YM5T02 = WNote1
037600020508     C                   EVAL      YM5T03 = WNote2
037700020508     C                   EVAL      YM5T04 = WNote3
037800020508     C                   EVAL      YM5T05 = WNote4
037900020508     C                   EVAL      YM5T06 = WNote5
038000020507     C                   EVAL      YM5FDE = VAOPOE
038100020507     C                   EVAL      YM5UDE = 'FED'
038200020507     C                   EXSR      sndptAS
038300020507     C                   ENDIF
038400020507     C*
038500020507     C* Verifico se richiesto incasso in fase di ritiro
038600020507     C                   IF        %subst(vindta:212:10) <> *blanks      AND
038700020507     C                             %subst(vindta:212:10) <> '0000000.00'
038800020507     C                   EVAL      YM5TES = 'ORM FEDEX (0891495) ' +
038900020507     C                                      'RIF:' + VAORFA
039000020508     C                   EVAL      YM5RIG = 3
039100020507     C                   EVAL      YM5T01 = '*** RICHIESTO INCASSO ***'
039200020507     C                   EVAL      YM5T02 = 'DIVISA:  ' + %subst(vindta:209:3)
039300020508     C                   EVAL      YM5T03 = 'IMPORTO: ' + %subst(vindta:212:10)
039400020507     C                   EVAL      YM5FDE = VAOPOE
039500020507     C                   EVAL      YM5UDE = 'FED'
039600020507     C                   EXSR      sndptAS
039700020507     C                   ENDIF
039800020507     C*
039900020507     C* Reperimento campi NUMERICI
040000020507     C*
040100020507     C* Data apertura chiamata
040200020507     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
040300020507     C                   EXSR      CHKNUM
040400020507     C                   IF        PiInt=*on
040500020507     C                   Z-ADD     PiVal         VAODAO
040600020507     C                   ELSE
040700020507     C                   SETON                                        32
040800020507     C                   EVAL      VAODAO = *zeros
040900020507     C                   EVAL      vinmsg = %trimr(vinmsg)
041000020507     C                             + ' ' + 'VAODAO'
041100020507     C                   ENDIF
041200020507     C*
041300020507     C* Ora apertura chiamata
041400020507     C                   EVAL      PiStr=%trim(%subst(vindta:12:4)) + '01'
041500020507     C                   EXSR      CHKNUM
041600020507     C                   IF        PiInt=*on
041700020507     C                   Z-ADD     PiVal         VAOOAO
041800020507     C                   ELSE
041900020507     C                   SETON                                        32
042000020507     C                   EVAL      VAOOAO = *zeros
042100020507     C                   EVAL      vinmsg = %trimr(vinmsg)
042200020507     C                             + ' ' + 'VAOOAO'
042300020507     C                   ENDIF
042400020507     C*
042500020507     C* CAP ritiro
042600020507     C                   EVAL      PiStr=%trim(%subst(vindta:136:9))
042700020507     C                   EXSR      CHKNUM
042800020507     C                   IF        PiInt=*on
042900020507     C                   Z-ADD     PiVal         Num5_0
043000020507     C                   MOVEL     Num5_0        VAOCAR
043100020507     C                   ELSE
043200020507     C                   SETON                                        32
043300020507     C                   EVAL      VAOCAR = *blanks
043400020507     C                   EVAL      vinmsg = %trimr(vinmsg)
043500020507     C                             + ' ' + 'VAOCAR'
043600020507     C                   ENDIF
043700020507     C*
043800020507     C* Data ritiro richiesta
043900020507     C                   EVAL      PiStr=%trim(%subst(vindta:16:8))
044000020507     C                   EXSR      CHKNUM
044100020507     C                   IF        PiInt=*on
044200020507     C                   Z-ADD     PiVal         VAODAR
044300020507     C                   ELSE
044400020507     C                   SETON                                        32
044500020507     C                   EVAL      VAODAR = *zeros
044600020507     C                   EVAL      vinmsg = %trimr(vinmsg)
044700020507     C                             + ' ' + 'VAODAR'
044800020507     C                   ENDIF
044900020507     C*
045000020507     C* Ora ritiro richiesta
045100020507     C                   EVAL      PiStr=%trim(%subst(vindta:36:4))
045200020507     C                   EXSR      CHKNUM
045300020507     C                   IF        PiInt=*on
045400020507     C                   Z-ADD     PiVal         VAOORR
045500020507     C                   ELSE
045600020507     C                   SETON                                        32
045700020507     C                   EVAL      VAOORR = *zeros
045800020507     C                   EVAL      vinmsg = %trimr(vinmsg)
045900020507     C                             + ' ' + 'VAOORR'
046000020507     C                   ENDIF
046100020507     C*
046200020507     C* Numero colli
046300020507     C                   EVAL      PiStr=%trim(%subst(vindta:197:5))
046400020507     C                   EXSR      CHKNUM
046500020507     C                   IF        PiInt=*on
046600020507     C                   Z-ADD     PiVal         VAONCL
046700020507     C                   ELSE
046800020507     C                   SETON                                        32
046900020507     C                   Z-ADD     *zeros        VAONCL
047000020507     C                   EVAL      vinmsg = %trimr(vinmsg)
047100020507     C                             + ' ' + 'VAONCL'
047200020507     C                   ENDIF
047300020507     C*
047400020507     C* Peso in kg.
047500020507     C                   EVAL      PiStr=%trim(%subst(vindta:202:7))
047600020507     C                   EVAL      PiDecChr = '.'
047700020507     C                   EXSR      CHKNUM
047800020507     C                   IF        PiNum=*on
047900020507     C                   Z-ADD(H)  PiVal         VAOPKG
048000020507     C                   ELSE
048100020507     C                   SETON                                        32
048200020507     C                   Z-ADD     *zeros        VAOPKG
048300020507     C                   EVAL      vinmsg = %trimr(vinmsg)
048400020507     C                             + ' ' + 'VAOPKG'
048500020507     C                   ENDIF
048600020507     C*
048700020507     C                   ENDSR
048800020507     C*----------------------------------------------------*
048900020507
049000020507
049100020507
049200020507     C*----------------------------------------------------*
049300020507     C*  CONTROLLO NUMERICITA' CAMPI
049400020507     C*----------------------------------------------------*
049500020507     C     CHKNUM        BEGSR
049600020507     C*
049700020507     C                   IF        PiDecChr = *blanks
049800020507     C                   EVAL      PiDecChr = '.'
049900020507     C                   ENDIF
050000020507     C*
050100020507     C                   CALL(e)   'ISNUMERIC'
050200020507     C                   PARM                    PiStr            30
050300020507     C                   PARM                    PiDecChr          1
050400020507     C                   PARM      *ZEROS        PiVal            30 9
050500020507     C                   PARM      '0'           PiInt             1
050600020507     C                   PARM      '0'           PiNum             1
050700020507     C                   IF        %error
050800020507     C                   EVAL      PiNum=*off
050900020507     C                   ENDIF
051000020507     C*
051100020507     C                   ENDSR
051200020507     C***
051300020507
051400020507
051500020507
051600020507     C*----------------------------------------------------*
051700020507     C*  INVIA MESSAGGIO DI POSTA AS
051800020507     C*----------------------------------------------------*
051900020507     C     SNDPTAS       BEGSR
052000020507     C*
052100130409     C***                CALL      'TNSYM5C'
052200130409     C***                PARM                    TNSYM5CDS
052300130409     C*
052400130409     C* invio e-mail con richiesta forzatura
052500130409     C                   EVAL      peml = 'fed089@brt.it'
052600130409     C                   EVAL      pcceml = *blanks
052700130409     C                   EVAL      pogg = YM5Tes
052800130409     C                   EVAL      pmsg =
052900130409     C                             YM5T01 +
053000130409     C                             ':/N' +
053100130409     C                             YM5T02 +
053200130409     C                             ':/N' +
053300130409     C                             YM5T03 +
053400130409     C                             ':/N' +
053500130409     C                             YM5T04 +
053600130409     C                             ':/N' +
053700130409     C                             YM5T05 +
053800130409     C                             ':/N' +
053900130409     C                             YM5T06 +
054000130409     C                             ':/N' +
054100130409     C                             YM5T07 +
054200130409     C                             ':/N' +
054300130409     C                             YM5T08 +
054400130409     C                             ':/N' +
054500130409     C                             YM5T09 +
054600130409     C                             ':/N' +
054700130409     C                             YM5T10 +
054800130409     C                             ':/N'
054900130409     C                   call      'TIS701C1'
055000130409     C                   parm                    peml            253
055100130409     C                   parm                    pcceml          253
055200130409     C                   parm                    pogg             44
055300130409     C                   parm                    pmsg           5000
055400130409     C                   parm                    pesito            1
055500020507     C*
055600020507     C                   ENDSR
055700020507     C***
055800030624
055900030624
056000030624     C*------------------------------------------------------------------------*
056100030624     C* CHKCOR - Considerazioni su VAOCOR
056200030624     C*------------------------------------------------------------------------*
056300030624     C     CHKCOR        BEGSR
056400030624     C*
056500030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
056600030624     C                   if        vaoTCO = 'F'
056700030624     C* Quindi verifico se il codice ordinante non è valorizzato
056800030625     C                   if        vaoCOR = *zeros  AND
056900030625     C                             vaoRSO = *blanks AND
057000030625     C                             vaoINO = *blanks AND
057100030625     C                             vaoCAO = *blanks AND
057200030625     C                             vaoLOO = *blanks AND
057300030625     C                             vaoPRO = *blanks AND
057400030625     C                             vaoNAO = *blanks
057500030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
057600030624     C                   movel     '0'           wFlgAcr           1
057700030624     C                   move      vlrKSC        wCli              7 0
057800030624     C                   move(p)   wCli          acrCRO
057900030624     C     acrCRO        setll     fnacr01l
058000030624     C                   if        %found(fnacr01l)
058100030624     C                   read      fnacr01l
058200030624     C                   dow       not %eof(fnacr01l)
058300030624     C                   movel     acrCRO        wCliAcr           7 0
058400030624     C                   if        wCliAcr = wCli
058500030624     C                   eval      vaoCOR = acrCRO
058600030624     C                   movel     '1'           wFlgAcr
058700030624     C                   leave
058800030624     C                   else
058900030624     C                   leave
059000030624     C                   endif
059100030624     C                   read      fnacr01l
059200030624     C                   enddo
059300030624     C                   endif
059400030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
059500030624     C* dati anagrafici dell'ordinante dal piano dei conti
059600030624     C                   if        wFlgAcr = '0'
059700030624     C                   clear                   BS69DS
059800030624     C                   clear                   ACODS
059900030624     C                   clear                   INDDS
060000030624     C                   clear                   CLPDS
060100030624     C                   clear                   CLSDS
060200030624     C                   move(P)   vlrKSC        I69KAC
060300030624     C                   move(P)   vlrKSC        I69KIN
060400030624     C                   call      'TIBS69R'
060500030624     C                   parm                    BS69DS
060600030624     C                   parm                    ACODS
060700030624     C                   parm                    INDDS
060800030624     C                   parm                    CLPDS
060900030624     C                   parm                    CLSDS
061000030624     C     O69ERR        ifne      '1'
061100030624     C                   eval      vaoRSO = ACORAG
061200030624     C                   eval      vaoINO = INDVIA
061300030624     C                   movel(P)  INDCAP        vaoCAO
061400030624     C                   eval      vaoLOO = INDCIT
061500030624     C                   eval      vaoPRO = INDPRV
061600030624     C                   eval      vaoNAO = INDSTA
061700030624     C                   endif
061800030624     C                   endif
061900030624     C                   endif
062000030624     C                   endif
062100030624     C*
062200030624     C                   ENDSR
062300020507
062400020507
062500010528
062600011105     C*------------------------------------------------------------------------*
062700011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
062800011105     C*------------------------------------------------------------------------*
062900011105     C     WRIORI        BEGSR
063000011105     C*
063100090618     C* Come prima cosa stacco un numeratore da AZNUM
063200090618     C                   clear                   trul33ds
063300090618     C                   eval      I33OPE = *zeros
063400090618     C                   eval      I33CNU = 600
063500090618     C                   eval      I33NUM = 1
063600090618     C                   movel     TRUL33DS      KPJBU
063700090618     C                   call      'TRUL33R'
063800090618     C                   parm                    KPJBA
063900090618     C                   movel     KPJBU         TRUL33DS
064000090618     C                   if        O33ERR = *zeros
064100090618     C                   z-add     O33NRF        §numori
064200090618     c                   else
064300090618     c                   eval      §numori = *all'9'
064400090618     C                   endif
064500011105     C*
064600011122     C* Quindi imposto i campi "extra-VAO"
064700011122     C                   if        vlrKSC = '0ORM0000'
064800011122     C*
064900011122     C                   if        vaoCOR > *zeros
065000011122     C                   movel     vaoCOR        wrkCOR7           7 0
065100011122     C*
065200011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
065300011122     C* codice padre. Se è un padre assegno già il codice ordinante.
065400011122     C                   clear                   tibs10ds
065500011122     C                   eval      d10drf = datcor                              *data riferimento
065600011122     C                   eval      d10paf = 'P'                                 *cerca il padre
065700011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
065800091026     C*
065900091026     C* Preparazione chiamata al TIBS10R1
066000091026     C                   clear                   tibs10r1ds
066100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
066200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
066300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
066400091026     C*
066500091026     C                   call      'TIBS10R1'
066600091026     C                   parm                    tibs10r1ds
066700091026     C*
066800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
066900011122     C*
067000011122     C                   if        d10err <> *blanks or                         *No unif. o padre
067100011122     C                             d10cop =  *zeros  or
067200011122     C                             d10cop =  d10cod
067300011122     C                   move      d10cod        oriIDC
067400011122     C                   else                                                   *Figlio
067500011122     C                   move      d10cop        oriIDC
067600011122     C                   endif
067700011122     C*
067800011122     C                   else
067900011122     C                   eval      oriIDC = *zeros
068000011122     C                   endif
068100011122     C*
068200011122     C                   else
068300011122     C                   eval      oriIDC = vlrKSC
068400011122     C                   endif
068500011122     C*
068600011105     C                   eval      oriPRG = §numori
068700011105     C                   eval      oriDER = datcor
068800011105     C                   eval      oriOER = wn6
068900011105     C                   eval      oriPOE = vaoPOE
069000011105     C                   eval      oriNSR = vaoNSR
069100011105     C                   eval      oriNOR = vaoNOR
069200011105     C                   eval      oriNRV = vaoNRV
069300011105     C                   eval      oriFLO = *blanks
069400011105     C*
069500011122     C* Infine valorizzo la chiave esterna sul file FNVAO
069600011105     C                   eval      dorm01 = vaoFLO
069700011105     C                   movel     §numori       §ormpg
069800030407     C                   movel     'F'           §ormic
069900011105     C                   eval      vaoFLO = dorm01
070000100204     C******             eval      %subst(VAOFLO:5:1) = *blanks
070100030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
070200011105     C*
070300011105     C* ...quindi scrivo il file TIORI00F
070400011105     C                   WRITE     TIORI000
070500011105     C*
070600011105     C                   ENDSR
070700020507     C***
070800050826
070900050826
071000050826
071100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
071200050826     C     wriVGD        BEGSR
071300050826     C*
071400050826     C* Reperisco la descrizione della fase da tabella
071500050826     C                   movel(p)  'FAR'         KteCOD
071600050826     C                   movel(p)  '000'         KteKE1
071700050826     C     KEYtbe        chain     tntbe01l
071800050826     C                   if        %found(tntbe01l)
071900050826     C                   movel(P)  tbeuni        DFAR
072000050826     C                   else
072100050826     C                   clear                   DFAR
072200050826     C                   endif
072300050826     C*
072400050826     C* Valorizzo buffer record
072500050826     C                   clear                   FNVAPDS
072600050826     C                   eval      vapIDC = oriIDC
072700050826     C                   eval      vapPOE = oriPOE
072800050826     C                   eval      vapRFA = vaoRFA
072900050826     C                   eval      vapPOG = oriPOE
073000050826     C                   eval      vapDAE = oriDER
073100050826     C                   eval      vapORE = oriOER
073200050826     C                   if        d§fardva <> *blanks
073300050826     C                   eval      vapDFA = d§fardva
073400050826     C                   else
073500050826     C                   eval      vapDFA = d§fardes
073600050826     C                   endif
073700050826     C*
073800050826     C                   clear                   tivgd000
073900080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
074000050826     C                   eval      vgdTIP = 'VP'
074100050826     C                   eval      vgdKSU = vapIDC
074200050826     C                   eval      vgdTSC = 'WW'
074300050826     C                   eval      vgdDAT = datcor
074400050826     C                   eval      vgdPGM = 'TITV12R'
074500050826     C                   write     tivgd000
074600050826     C*
074700050826     C                   ENDSR
074800050826     C*------------------------------------------------------------------------*
074900011119
075000010528
075100010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
075200010528     C     repfil        BEGSR
075300010528     C*
075400010528     C                   if        invfil = *zeros and
075500010528     C                             depfil > *zeros and
075600010613     C                             (vinflg = *blanks or
075700010613     C                              vinflg = *zeros)
075800010528     C
075900010528     C                   eval      invfil = depfil
076000010528     C                   endif
076100010528     C*
076200010529     C                   if        depfil <> invfil and
076300010529     C                             invfil > *zeros
076400010528     C                   eval      flgMulti = '1'
076500010528     C                   if        vinflg = *blanks
076600010528     C                   add       1             cntNonEl
076700010528     C                   endif
076800010528     C                   endif
076900010528     C*
077000010528     C                   if        vinflg = '2'
077100010528     C                   eval      flgStato = '2'
077200010528     C                   endif
077300010528     C*
077400010528     C                   ENDSR
077500010528     C***
077600010528
077700010528
077800010528
077900010528
078000990920      /TITLE Invio dei dati al punto operativo.
078100000613     C     invio         BEGSR
078200990920     C*
078300990920     C                   reset                   dscmz
078400010528     C                   move      invfil        cmzdst
078500990920     C                   eval      cmzfld = vlrfou
078600990920     C                   eval      cmzmbd = vlrhdl
078700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
078800000710     C                   if        prmfir = *blanks
078900010122     C                   eval      cmzfla = 'FNVAO00F'
079000010122     C                   eval      cmzmba = 'FNVAO00F'
079100000710     C                   else
079200000710     C                   eval      cmzfla = prmfir
079300000710     C                   eval      cmzmba = prmfir
079400000710     C                   endif
079500990920     C                   eval      cmznrr = *zeros
079600990920     C                   move      §ctrok        cmznrr
079700010123     C                   eval      cmzlba = vlrfl1
079800990920     C                   call(e)   'TIS711C'
079900990920     C                   parm                    dscmz
080000990921     C                   parm      *blanks       esito
080100990920     C                   if        %error
080200990920     C                             or cmzerr = '1'
080300990921     C                             or esito  = '1'
080400000710     C                   eval      wrkesito = '3'
080500990920     C                   endif
080600990920     C*
080700000613     C                   ENDSR
080800000613     C***
080900990910
081000010528
081100010528
081200010528
081300010528      /TITLE Invio dei dati al punto operativo.
081400010528     C     opeini        BEGSR
081500010528     C*
081600010528     C* Inizializzo flag e contatori operativi
081700010528     C                   movel     '0'           flgGiro           1
081800010528     C                   movel     '0'           flgMulti          1
081900010528     C                   movel     '1'           flgStato          1
082000010615     C                   movel     '0'           flgOk             1
082100010528     C                   z-add     *zeros        cntNonEl         10 0
082200010528     C                   z-add     *zeros        depfil            3 0
082300010528     C                   z-add     *zeros        invfil            3 0
082400010528     C*
082500010528     C                   ENDSR
082600010528     C***
082700010528
082800010528
082900010528
083000010528
083100000613     C     *inzsr        BEGSR
083200990910     C*
083300990910     C     *entry        plist
083400990920     C                   parm                    tivlrds
083500990921     C                   parm      wrkesito      esito
083600000724     C                   parm                    prmlit
083700000710     C                   parm                    prmfir
083800011105     C*
083900011105     C* Campi riferito al database
084000011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
084100011105     C     *like         define    tbeke1        kteKE1
084200011105     C*
084300011105     C* Definizione chiavi di lettura
084400011105     C* TNTBE01L - parziale
084500011105     C     keytbe        klist
084600011105     C                   kfld                    kteCOD                         *UTENTE
084700011105     C                   kfld                    kteKE1                         *CODICE
084800011105     C*
084900011105     C* Reperimento data e ora corrente
085000011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
085100011105     C                   movel     wn14          wn6               6 0          *ORA (6)
085200011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
085300011105     C                   z-add     wn8           g08dat
085400011105     C                   z-add     *zeros        g08inv
085500011105     C                   movel     '0'           g08err
085600011105     C                   call      'XSRDA8'
085700011105     C                   parm                    wlbda8
085800011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
085900010529     C*
086000000613     C                   ENDSR
086100000613     C***
