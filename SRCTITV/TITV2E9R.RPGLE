000100110504      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente CEVA - VODAFONE cod. 0505148)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500030624     Ffnacr01l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800010122     FFNVAOwwr  O    E             DISK    usropn
000900011105     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100110502     Ftitv2e9p  O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300110502     Ftitv2e9ps O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500990908
001600030624     D*------------------
001700030624     D* DS REPERIMENTO DATI CLIENTE
001800030624     D*-------------------
001900030624     D BS69DS        E DS                  EXTNAME(TIBS69DS)
002000030624     D ACODS         E DS                  EXTNAME(CNACO00F)
002100030624     D INDDS         E DS                  EXTNAME(CNIND00F)
002200030624     D CLPDS         E DS                  EXTNAME(CNCLP00F)
002300030624     D CLSDS         E DS                  EXTNAME(FNCLS00F)
002400000512     D*------------
002500000512     D* COMANDI
002600000512     D*------------
002700011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002800000512     D*------------
002900990920     D dscmz         e ds                  inz
003000990910     D psds           sds
003100990910     D  procname         *PROC
003200010122     D*
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400050826     D fnvapds       e ds                  extname(fnvap00f)
003500071016     D tisi95ds      e ds
003600011105     D dorm01        e ds
003700011119     D dfar          e ds
003800110502     D trul33ds      e ds
003900110502     D kpjba         e ds
004000990910     D esito           s              1
004100000724     D prmlit          s             10
004200000710     D prmfir          s             10
004300990921     D wrkesito        s                   like(esito)
004400990915     D wrkdata         s               d
004500990915     D wrkora          s               t
004600000613     D rrnum           s              6  0 INZ(*zeros)
004700000621     D recko           s            150    INZ(*blanks)
004800011123     D depcmd          s            150    INZ(*blanks)
004900020507     D wrkRFA          s                   LIKE(vaorfa)
005000110502     D §numori         s              7  0 inz
005100011122     D*-------------------
005200010731     D* COSTANTI
005300011122     D*-------------------
005400010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005500010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005600990908
005700091026
005800091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
005900091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006000091026
006100010528
006200010528
006300010528
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000724     C*
007000010528     C                   exsr      opeini
007100010528     C                   exsr      rwvao
007200010528     C*
007300010528     C                   seton                                        lr
007400010528
007500010528
007600010528
007700010528
007800010528     C*--------------------------------------------------------
007900010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010528     C*--------------------------------------------------------
008100010528     C     PREELA        BEGSR
008200010528     C*
008300000724     C* SE OCCORRE SPEDIRE IN FILIALE
008400010528     C                   if        invfil <> *zeros and
008500010528     C                             flgGiro = '0'
008600010528     C*
008700010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010528     C                   eval      flgGiro = '1'
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200010529     C     invfil        chain     azorg01l
009300000724     C                   if        %found
009400000616     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011123     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000616     C*
010500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010600000616     C                   movel(p)  CMD(2)        depcmd
010700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010800000616     C*
010900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011000011123     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    depcmd
011300000616     C                   PARM                    LENGH
011400000616     C*
011500110502     C                   if        not %open(titv2e9ps)
011600110502     C                   open      titv2e9ps
011700000616     C                   except    testdett
011800010528     C                   endif
011900010529     C*
012000010529     C                   endif
012100010528     C*
012200010528     C                   ENDSR
012300010528     C***
012400010528
012500010528
012600010528     C*--------------------------------------------------------
012700010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010528     C*--------------------------------------------------------
012900010528     C     ENDELA        BEGSR
013000000613     C*
013100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013200000613     C*
013300110502     C                   if        %open(titv2e9ps)
013400000616     C                   except    findett
013500110502     C                   close     titv2e9ps
013600000613     C                   endif
013700000616     C*
013800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013900011123     C                   Z-ADD     150           LENGH            15 5
014000010529     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014100000616     C                   PARM                    CMD(3)
014200000616     C                   PARM                    LENGH
014300000613     C*
014400010528     C                   ENDSR
014500010528     C***
014600010528
014700000613
014800000613
014900000613     C*--------------------------------------------------------
015000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015100000613     C*--------------------------------------------------------
015200000621     C     STPR          BEGSR
015300000613     C*
015400110502     C                   if        not %open(titv2e9p)
015500110502     C                   open      titv2e9p
015600990915     C                   endif
015700990915     C*
015800990915     C                   except    riepilogo
015900990915     C*
016000110502     C                   if        %open(titv2e9p)
016100110502     C                   close     titv2e9p
016200990914     C                   endif
016300990910     C*
016400000613     C                   ENDSR
016500000613     C***
016600990908
016700010528
016800010528
016900910830     C*--------------------------------------------------------
017000020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
017100910830     C*--------------------------------------------------------
017200010122     C     RWVAO         BEGSR
017300010528     C*
017400990914     C                   if        not %open(tivin00r)
017500990908     C                   open      tivin00r
017600990914     C                   endif
017700010122     C                   if        not %open(fnvaowwr)
017800010122     C                   open      fnvaowwr
017900990914     C                   endif
018000990910     C*
018100990910     C                   clear                   §CTROK
018200990910     C                   clear                   §CTRMO
018300990910     C                   clear                   §CTRNO
018400990910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800010731     C*
018900010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019000010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019100010618     C*
019200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019300010618     C                   if        vinflg = '1'
019400010618     C                   eval      flgOk = '1'
019500010618     C                   endif
019600010618     C*
019700000905     C                   if        vindta > *blanks
019800000613     C                   add       1             rrnum
019900990913     C*
020000010711     C                   if        *in70 = *off and
020100010711     C                             (vinflg = *blanks
020200010711     C                              or vinflg = '0'
020300010711     C                              or vinflg = '2')
020400990913     C*
020500010528     C                   clear                   FNVAO000
020600011105     C                   clear                   TIORI000
020700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020800010711     C                   if        vinflg = *blanks or vinflg = '0'
020900010711     C                   clear                   vinmsg
021000010711     C                   endif
021100020507     C*
021200020507     C                   exsr      inzvar
021300020527     C                   exsr      defcam
021400020507     C                   exsr      impvao
021500010122     C*
021600010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700010528     C                   eval      depfil = VAOPOE
021800010528     C                   exsr      repfil
021900020507     C                   if        depfil = invfil
022000010528     C*
022100010528     C                   exsr      PREELA
022200010710     C*
022300930409     C*
022400010604     C  N31              ADD       1             §CTROK            7 0
022500010604     C   32              ADD       1             §CTRMO            7 0
022600010604     C   31              ADD       1             §CTRNO            7 0
022700020507     C*
022800030624     C  N31              EXSR      CHKCOR
022900011105     C  N31              EXSR      WRIORI
023000110503     C* N31              EXSR      WRIVGD
023100010122     C  N31              WRITE     FNVAO000
023200990910     C*
023300010604     C                   if        *in31 = *off and
023400010604     C                             *in32 = *off
023500990910     C                   eval      vinflg = '1'
023600990910     C                   else
023700000621     C                   eval      recko = vindta
023800110502     C                   if        %open(titv2e9ps)
023900000616     C                   except    rigadett
024000010612     C                   endif
024100990910     C                   eval      vinflg = '2'
024200991022     C                   endif
024300990910     C                   endif
024400010604     C*
024500010604     C                   endif
024600000905     C*
024700000905     C                   else
024800000905     C                   eval      vinflg = '1'
024900000905     C                   endif
025000000905     C*
025100000905     C  N70              update    tivin000
025200991022     C*
025300991022     C  N70              ENDdo
025400010528     C*
025500010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025600010528     C                   if        cntNonEl = *zeros or
025700010528     C                             flgMulti = '0'
025800010528     C* Se non ci sono record con errori ...
025900010528     C                   if        §ctrno = 0 and
026000010604     C                             §ctrmo = 0 and
026100010528     C                             flgStato <> '2'
026200990910     C* ... restituisco esito OK.
026300990921     C                   eval      wrkesito = '0'
026400990910     C                   else
026500000710     C                   if        §ctrok > 0
026600990921     C                   eval      wrkesito = '1'
026700000710     C                   else
026800010615     C                   if        flgOk = '0'
026900010615     C                   eval      wrkesito = '2'
027000010615     C                   else
027100010615     C                   eval      wrkesito = '6'
027200010615     C                   endif
027300990910     C                   endif
027400010528     C                   endif
027500010529     C                   else
027600010529     C                   eval      wrkesito = '9'
027700000710     C                   endif
027800990910     C*
027900990914     C                   if        %open(tivin00r)
028000990908     C                   close     tivin00r
028100990914     C                   endif
028200010122     C                   if        %open(fnvaowwr)
028300010122     C                   close     fnvaowwr
028400990914     C                   endif
028500990910     C*
028600010528     C                   if        vlrpoi <> 999
028700010528     C                   eval      invfil = vlrpoi
028800010528     C                   endif
028900010528     C*
029000990920     C                   if        §ctrok > 0
029100010528     C                             and invfil > *zeros
029200000613     C                   exsr      invio
029300990920     C                   endif
029400010612     C*
029500010612     C                   if        flgGiro = '1'
029600010612     C                   exsr      endela
029700010612     C                   endif
029800010528     C*
029900910830     C                   ENDSR
030000000613     C***
030100990920
030200020507
030300020507     C*----------------------------------------------------*
030400020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030500020507     C*----------------------------------------------------*
030600020507     C     INZVAR        BEGSR
030700020507     C*
030800020507     C                   Z-ADD     *zeros        Num5_0            5 0
030900020507     C                   Z-ADD     *zeros        Num3_0            3 0
031000020507     C*
031100020507     C                   ENDSR
031200020507     C*----------------------------------------------------*
031300020507     C*  IMPOSTAZIONE CAMPI COSTANTI
031400020507     C*----------------------------------------------------*
031500020507     C     DEFCAM        BEGSR
031600020507     C*
031700020507     C* Imposto i valori di default...
031800110502     C                   EVAL      VAOPOE = 050
031900020507     C                   EVAL      VAOTOR = 'S'
032000020507     C                   EVAL      VAOTCO = 'F'
032100110502     C                   EVAL      VAOCOR = 0505148000
032200020507     C                   EVAL      VAOFFD = *blanks
032300110502     C                   EVAL      VAOPAG = 'O'
032400110502     C                   EVAL      VAOKSC = 0505148
032500110503     C                   EVAL      VAODDT = 'S'
032600110712     C                   EVAL      VAOCTR = '001'
032700110502     C                   EVAL      VAOKSC = 0505148
032800110504     C                   EVAL      VAOPOC = 050
032900110502     C                   EVAL      ORIIDC = '00505148'
033000020507     C* ... e poi verifico se sono stati passati come parametri
033100020507     C                   IF        vlrppt > *blanks
033200020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
033300020507     C                   EXSR      CHKNUM
033400020507     C                   IF        PiInt=*on
033500020507     C                   Z-ADD     PiVal         VAOCOR
033600020507     C                   Z-ADD     PiVal         VAOCRC
033700110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
033800020507     C                   ENDIF
033900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034000020507     C                   EXSR      CHKNUM
034100020507     C                   IF        PiInt=*on
034200020507     C                   Z-ADD     PiVal         VAOPOE
034300020507     C                   Z-ADD     PiVal         VAOPOC
034400020507     C                   ENDIF
034500020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
034600020507     C                   EXSR      CHKNUM
034700020507     C                   IF        PiInt=*on
034800020507     C                   Z-ADD     PiVal         Num3_0
034900020507     C                   MOVEL     NUM3_0        VAOCTR
035000020507     C                   ENDIF
035100020507     C                   ENDIF
035200020507     C*
035300020507     C                   ENDSR
035400020507     C*----------------------------------------------------*
035500020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
035600020507     C*----------------------------------------------------*
035700020507     C     IMPVAO        BEGSR
035800020507     C*
035900020507     C                   SETOFF                                       3132
036000020507     C*
036100020507     C* Reperimento campi ALFA
036200020507     C*
036300020507     C* Considerazioni sulla ragione sociale del destinatario da indicare
036400110502     C                   EVAL      VAORSR=%trim(%subst(vindta:22:30))
036500020507     C* == verifico se esiste il carattere @e, se c'è diventa A
036600020507     C     '@':'A'       XLATE     VAORSR        VAORSR
036700020507     C* ==
036800110502     C                   EVAL      VAOINR=%trim(%subst(vindta:82:30))
036900110502     C                   EVAL      VAOLOR=%trim(%subst(vindta:52:30))
037000110502     C                   EVAL      VAOPRR=%trim(%subst(vindta:120:5))
037100110502     C*
037200110502     C                   EVAL      VAORSC=%trim(%subst(vindta:156:30))
037300110502     C* == verifico se esiste il carattere @e, se c'è diventa A
037400110502     C     '@':'A'       XLATE     VAORSC        VAORSC
037500110502     C* ==
037600110502     C                   EVAL      VAOINC=%trim(%subst(vindta:216:30))
037700110502     C                   EVAL      VAOLOC=%trim(%subst(vindta:186:30))
037800110502     C                   EVAL      VAOPRC=%trim(%subst(vindta:254:5))
037900110502     C*
038000110502     C                   EVAL      VAORFA=%trim(%subst(vindta:125:20))
038100110502     C                   SELECT
038200110502     C                   WHEN      %subst(vindta:10:1) = '1'
038300110502     C                   EVAL      VAORMP = 'M'
038400110502     C                   WHEN      %subst(vindta:10:1) = '2'
038500110502     C                   EVAL      VAORMP = 'P'
038600110502     C                   WHEN      %subst(vindta:10:1) = '3'
038700110502     C                   EVAL      VAORMP = 'M'
038800110502     C                   WHEN      %subst(vindta:10:1) = '4'
038900110502     C                   EVAL      VAORMP = 'P'
039000110502     C                   ENDSL
039100020507     C*
039200020507     C* Reperimento campi NUMERICI
039300020507     C*
039400020527     C* Data e ora apertura chiamata
039500020527     C                   Z-ADD     datcor        VAODAO
039600071016     C                   MOVEL     wn14          VAOOAO
039700020507     C*
039800020507     C* CAP ritiro
039900110502     C                   EVAL      PiStr=%trim(%subst(vindta:112:8))
040000020507     C                   EXSR      CHKNUM
040100020507     C                   IF        PiInt=*on
040200020507     C                   Z-ADD     PiVal         Num5_0
040300020507     C                   MOVEL     Num5_0        VAOCAR
040400020507     C                   ELSE
040500020507     C                   SETON                                        32
040600020507     C                   EVAL      VAOCAR = *blanks
040700020507     C                   EVAL      vinmsg = %trimr(vinmsg)
040800020507     C                             + ' ' + 'VAOCAR'
040900020507     C                   ENDIF
041000110502     C*
041100110502     C* CAP consegna
041200110502     C                   EVAL      PiStr=%trim(%subst(vindta:246:8))
041300110502     C                   EXSR      CHKNUM
041400110502     C                   IF        PiInt=*on
041500110502     C                   Z-ADD     PiVal         Num5_0
041600110502     C                   MOVEL     Num5_0        VAOCAC
041700110502     C                   ELSE
041800110502     C                   SETON                                        32
041900110502     C                   EVAL      VAOCAC = *blanks
042000110502     C                   EVAL      vinmsg = %trimr(vinmsg)
042100110502     C                             + ' ' + 'VAOCAC'
042200110502     C                   ENDIF
042300020507     C*
042400020507     C* Data ritiro richiesta
042500110502     C                   EVAL      PiStr=%subst(vindta:2:8)
042600020507     C                   EXSR      CHKNUM
042700020507     C                   IF        PiInt=*on
042800020507     C                   Z-ADD     PiVal         VAODAR
042900020507     C                   ELSE
043000020507     C                   SETON                                        32
043100020507     C                   EVAL      VAODAR = *zeros
043200020507     C                   EVAL      vinmsg = %trimr(vinmsg)
043300020507     C                             + ' ' + 'VAODAR'
043400020507     C                   ENDIF
043500071012     C*
043600071012     C* Colli
043700110502     C                   EVAL      PiStr=%subst(vindta:259:5)
043800071012     C                   EXSR      CHKNUM
043900071012     C                   IF        PiInt=*on
044000071012     C                   Z-ADD     PiVal         VAONCL
044100071012     C                   ELSE
044200071012     C                   SETON                                        32
044300071012     C                   EVAL      VAONCL = *zeros
044400071012     C                   EVAL      vinmsg = %trimr(vinmsg)
044500071012     C                             + ' ' + 'VAONCL'
044600071012     C                   ENDIF
044700071012     C*
044800071012     C* Peso Kg
044900110502     C                   EVAL      PiStr=%subst(vindta:264:7)
045000071012     C                   EXSR      CHKNUM
045100071012     C                   IF        PiNum=*on
045200110502     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
045300071012     C                   Z-ADD     PiVal         VAOPKG
045400071012     C                   ELSE
045500071012     C                   SETON                                        32
045600071012     C                   EVAL      VAOPKG = *zeros
045700071012     C                   EVAL      vinmsg = %trimr(vinmsg)
045800071012     C                             + ' ' + 'VAOPKG'
045900071012     C                   ENDIF
046000110502     C*
046100110502     C* Volume M3
046200110630     C                   EVAL      PiStr=%subst(vindta:271:4)
046300110502     C                   EXSR      CHKNUM
046400110502     C                   IF        PiNum=*on
046500110502     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
046600110502     C                   Z-ADD     PiVal         VAOVLM
046700110502     C                   ELSE
046800110502     C                   SETON                                        32
046900110502     C                   EVAL      VAOVLM = *zeros
047000110502     C                   EVAL      vinmsg = %trimr(vinmsg)
047100110502     C                             + ' ' + 'VAOVLM'
047200110502     C                   ENDIF
047300020507     C*
047400020507     C                   ENDSR
047500020507     C*----------------------------------------------------*
047600020507
047700020507
047800020507
047900020507     C*----------------------------------------------------*
048000020507     C*  CONTROLLO NUMERICITA' CAMPI
048100020507     C*----------------------------------------------------*
048200020507     C     CHKNUM        BEGSR
048300020507     C*
048400020507     C                   IF        PiDecChr = *blanks
048500020507     C                   EVAL      PiDecChr = '.'
048600020507     C                   ENDIF
048700020507     C*
048800020507     C                   CALL(e)   'ISNUMERIC'
048900020507     C                   PARM                    PiStr            30
049000020507     C                   PARM                    PiDecChr          1
049100020507     C                   PARM      *ZEROS        PiVal            30 9
049200020507     C                   PARM      '0'           PiInt             1
049300020507     C                   PARM      '0'           PiNum             1
049400020507     C                   IF        %error
049500020507     C                   EVAL      PiNum=*off
049600020507     C                   ENDIF
049700020507     C*
049800020507     C                   ENDSR
049900020507     C***
050000030624
050100030624
050200030624     C*------------------------------------------------------------------------*
050300030624     C* CHKCOR - Considerazioni su VAOCOR
050400030624     C*------------------------------------------------------------------------*
050500030624     C     CHKCOR        BEGSR
050600030624     C*
050700030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
050800030624     C                   if        vaoTCO = 'F'
050900030624     C* Quindi verifico se il codice ordinante non è valorizzato
051000030625     C                   if        vaoCOR = *zeros  AND
051100030625     C                             vaoRSO = *blanks AND
051200030625     C                             vaoINO = *blanks AND
051300030625     C                             vaoCAO = *blanks AND
051400030625     C                             vaoLOO = *blanks AND
051500030625     C                             vaoPRO = *blanks AND
051600030625     C                             vaoNAO = *blanks
051700030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
051800030624     C                   movel     '0'           wFlgAcr           1
051900030624     C                   move      vlrKSC        wCli              7 0
052000030624     C                   move(p)   wCli          acrCRO
052100030624     C     acrCRO        setll     fnacr01l
052200030624     C                   if        %found(fnacr01l)
052300030624     C                   read      fnacr01l
052400030624     C                   dow       not %eof(fnacr01l)
052500030624     C                   movel     acrCRO        wCliAcr           7 0
052600030624     C                   if        wCliAcr = wCli
052700030624     C                   eval      vaoCOR = acrCRO
052800030624     C                   movel     '1'           wFlgAcr
052900030624     C                   leave
053000030624     C                   else
053100030624     C                   leave
053200030624     C                   endif
053300030624     C                   read      fnacr01l
053400030624     C                   enddo
053500030624     C                   endif
053600030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
053700030624     C* dati anagrafici dell'ordinante dal piano dei conti
053800030624     C                   if        wFlgAcr = '0'
053900030624     C                   clear                   BS69DS
054000030624     C                   clear                   ACODS
054100030624     C                   clear                   INDDS
054200030624     C                   clear                   CLPDS
054300030624     C                   clear                   CLSDS
054400030624     C                   move(P)   vlrKSC        I69KAC
054500030624     C                   move(P)   vlrKSC        I69KIN
054600030624     C                   call      'TIBS69R'
054700030624     C                   parm                    BS69DS
054800030624     C                   parm                    ACODS
054900030624     C                   parm                    INDDS
055000030624     C                   parm                    CLPDS
055100030624     C                   parm                    CLSDS
055200030624     C     O69ERR        ifne      '1'
055300030624     C                   eval      vaoRSO = ACORAG
055400030624     C                   eval      vaoINO = INDVIA
055500030624     C                   movel(P)  INDCAP        vaoCAO
055600030624     C                   eval      vaoLOO = INDCIT
055700030624     C                   eval      vaoPRO = INDPRV
055800030624     C                   eval      vaoNAO = INDSTA
055900030624     C                   endif
056000030624     C                   endif
056100030624     C                   endif
056200030624     C                   endif
056300030624     C*
056400030624     C                   ENDSR
056500020507
056600020507
056700010528
056800011105     C*------------------------------------------------------------------------*
056900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
057000011105     C*------------------------------------------------------------------------*
057100011105     C     WRIORI        BEGSR
057200011105     C*
057300090617     C* Come prima cosa stacco un numeratore da AZNUM
057400090617     C                   clear                   trul33ds
057500090617     C                   eval      I33OPE = *zeros
057600090617     C                   eval      I33CNU = 600
057700090617     C                   eval      I33NUM = 1
057800090617     C                   movel     TRUL33DS      KPJBU
057900090617     C                   call      'TRUL33R'
058000090617     C                   parm                    KPJBA
058100090617     C                   movel     KPJBU         TRUL33DS
058200090617     C                   if        O33ERR = *zeros
058300090617     C                   z-add     O33NRF        §numori
058400090618     c                   else
058500090618     c                   eval      §numori = *all'9'
058600090617     C                   endif
058700011105     C*
058800110502     C* Quindi imposto i campi "extra-VAO"
058900011105     C                   eval      oriPRG = §numori
059000011105     C                   eval      oriDER = datcor
059100110502     C                   eval      oriOER = oracor
059200011105     C                   eval      oriPOE = vaoPOE
059300011105     C                   eval      oriNSR = vaoNSR
059400011105     C                   eval      oriNOR = vaoNOR
059500011105     C                   eval      oriNRV = vaoNRV
059600011105     C                   eval      oriFLO = *blanks
059700011105     C*
059800011122     C* Infine valorizzo la chiave esterna sul file FNVAO
059900011105     C                   eval      dorm01 = vaoFLO
060000011105     C                   movel     §numori       §ormpg
060100110502     C                   movel     'S'           §ormfr
060200110502     C                   movel     'S'           §ormks
060300110502     C                   movel     'S'           §ormfd
060400110503     C                   movel     'N'           §orcomc
060500011105     C                   eval      vaoFLO = dorm01
060600030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
060700030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
060800011105     C*
060900011105     C* ...quindi scrivo il file TIORI00F
061000011105     C                   WRITE     TIORI000
061100011105     C*
061200011105     C                   ENDSR
061300020507     C***
061400050826
061500050826
061600050826
061700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
061800050826     C     wriVGD        BEGSR
061900050826     C*
062000050826     C* Reperisco la descrizione della fase da tabella
062100050826     C                   movel(p)  'FAR'         KteCOD
062200050826     C                   movel(p)  '000'         KteKE1
062300050826     C     KEYtbe        chain     tntbe01l
062400050826     C                   if        %found(tntbe01l)
062500050826     C                   movel(P)  tbeuni        DFAR
062600050826     C                   else
062700050826     C                   clear                   DFAR
062800050826     C                   endif
062900050826     C*
063000050826     C* Valorizzo buffer record
063100050826     C                   clear                   FNVAPDS
063200050826     C                   eval      vapIDC = oriIDC
063300050826     C                   eval      vapPOE = oriPOE
063400050826     C                   eval      vapRFA = vaoRFA
063500050826     C                   eval      vapPOG = oriPOE
063600050826     C                   eval      vapDAE = oriDER
063700050826     C                   eval      vapORE = oriOER
063800050826     C                   if        d§fardva <> *blanks
063900050826     C                   eval      vapDFA = d§fardva
064000050826     C                   else
064100050826     C                   eval      vapDFA = d§fardes
064200050826     C                   endif
064300050826     C*
064400050826     C                   clear                   tivgd000
064500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
064600050826     C                   eval      vgdTIP = 'VP'
064700050826     C                   eval      vgdKSU = vapIDC
064800050826     C                   eval      vgdTSC = 'WW'
064900050826     C                   eval      vgdDAT = datcor
065000110502     C                   eval      vgdPGM = 'TITV2E9R'
065100050826     C                   write     tivgd000
065200050826     C*
065300050826     C                   ENDSR
065400050826     C*------------------------------------------------------------------------*
065500011119
065600010528
065700010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
065800010528     C     repfil        BEGSR
065900010528     C*
066000010528     C                   if        invfil = *zeros and
066100010528     C                             depfil > *zeros and
066200010613     C                             (vinflg = *blanks or
066300010613     C                              vinflg = *zeros)
066400010528     C
066500010528     C                   eval      invfil = depfil
066600010528     C                   endif
066700010528     C*
066800010529     C                   if        depfil <> invfil and
066900010529     C                             invfil > *zeros
067000010528     C                   eval      flgMulti = '1'
067100010528     C                   if        vinflg = *blanks
067200010528     C                   add       1             cntNonEl
067300010528     C                   endif
067400010528     C                   endif
067500010528     C*
067600010528     C                   if        vinflg = '2'
067700010528     C                   eval      flgStato = '2'
067800010528     C                   endif
067900010528     C*
068000010528     C                   ENDSR
068100010528     C***
068200010528
068300010528
068400010528
068500010528
068600990920      /TITLE Invio dei dati al punto operativo.
068700000613     C     invio         BEGSR
068800990920     C*
068900990920     C                   reset                   dscmz
069000010528     C                   move      invfil        cmzdst
069100990920     C                   eval      cmzfld = vlrfou
069200990920     C                   eval      cmzmbd = vlrhdl
069300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
069400000710     C                   if        prmfir = *blanks
069500010122     C                   eval      cmzfla = 'FNVAO00F'
069600010122     C                   eval      cmzmba = 'FNVAO00F'
069700000710     C                   else
069800000710     C                   eval      cmzfla = prmfir
069900000710     C                   eval      cmzmba = prmfir
070000000710     C                   endif
070100990920     C                   eval      cmznrr = *zeros
070200990920     C                   move      §ctrok        cmznrr
070300010123     C                   eval      cmzlba = vlrfl1
070400990920     C                   call(e)   'TIS711C'
070500990920     C                   parm                    dscmz
070600990921     C                   parm      *blanks       esito
070700990920     C                   if        %error
070800990920     C                             or cmzerr = '1'
070900990921     C                             or esito  = '1'
071000000710     C                   eval      wrkesito = '3'
071100990920     C                   endif
071200990920     C*
071300000613     C                   ENDSR
071400000613     C***
071500990910
071600010528
071700010528
071800010528
071900010528      /TITLE Invio dei dati al punto operativo.
072000010528     C     opeini        BEGSR
072100010528     C*
072200010528     C* Inizializzo flag e contatori operativi
072300010528     C                   movel     '0'           flgGiro           1
072400010528     C                   movel     '0'           flgMulti          1
072500010528     C                   movel     '1'           flgStato          1
072600010615     C                   movel     '0'           flgOk             1
072700010528     C                   z-add     *zeros        cntNonEl         10 0
072800010528     C                   z-add     *zeros        depfil            3 0
072900010528     C                   z-add     *zeros        invfil            3 0
073000010528     C*
073100010528     C                   ENDSR
073200010528     C***
073300010528
073400010528
073500010528
073600010528
073700000613     C     *inzsr        BEGSR
073800990910     C*
073900990910     C     *entry        plist
074000990920     C                   parm                    tivlrds
074100990921     C                   parm      wrkesito      esito
074200000724     C                   parm                    prmlit
074300000710     C                   parm                    prmfir
074400011105     C*
074500011105     C* Campi riferito al database
074600011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
074700011105     C     *like         define    tbeke1        kteKE1
074800011105     C*
074900011105     C* Definizione chiavi di lettura
075000011105     C* TNTBE01L - parziale
075100011105     C     keytbe        klist
075200011105     C                   kfld                    kteCOD                         *UTENTE
075300011105     C                   kfld                    kteKE1                         *CODICE
075400011105     C*
075500011105     C* Reperimento data e ora corrente
075600110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
075700110502     C                   movel     wn14          oracor            6 0          *ORA (6)
075800110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075900110502     C                   eval      datcor = %dec(%date() : *ISO)
076000010529     C*
076100000613     C                   ENDSR
076200000613     C***
076300990908
076400110502     Otitv2e9p  E            riepilogo         2
076500990915     O                                              'Upload via Internet'
076600990915     O                                           +1 'Traduzione TIVIN00R -'
076700010122     O                                           +1 'FNVAOWWR'
076800010122     O                                           +1 'Ordini ritiro merce'
076900990915     O          E            riepilogo   2
077000990915     O                       wrkdata
077100990915     O                       wrkora              +1
077200990915     O                       procname            +1
077300990915     O          E            riepilogo   2
077400990915     O                                              'Cliente..................:'
077500010122     O                       VAOKSC        z     +1
077600990915     O          E            riepilogo   2
077700990920     O                                              'Riferimento Strategi.....:'
077800990920     O                       vlrhdl              +1
077900990915     O          E            riepilogo   2
078000990915     O                                              'Giusti...................:'
078100971022     O                       §CTROK        2   +  1
078200990915     O          E            riepilogo   2
078300990915     O                                              'Sbagliati e corretti.....:'
078400971022     O                       §CTRMO        2   +  1
078500990915     O          E            riepilogo   2
078600990915     O                                              'Sbagliati e scartati.....:'
078700971022     O                       §CTRNO        2   +  1
078800000613
078900110502     Otitv2e9ps E            testdett          2
079000000613     O                                              'Upload via Internet'
079100000613     O                                           +1 'Traduzione TIVIN00R -'
079200010122     O                                           +1 'FNVAOWWR'
079300010122     O                                           +1 'Ordini ritiro merce'
079400000616     O          E            testdett    3
079500000613     O                                           +2 'N° rec'
079600000613     O                                           +3 'Anteprima contenuto'
079700000616     O          E            rigadett    2
079800000613     O                       rrnum               +2
079900000621     O                       recko               +3
080000000616     O          E            findett     2
080100000613     O                       wrkdata
080200000613     O                       wrkora              +1
080300000613     O                       procname            +1
080400000616     O          E            findett     2
080500000613     O                                              'Cliente..................:'
080600010122     O                       VAOKSC        z     +1
080700000616     O          E            findett     2
080800000613     O                                              'Riferimento Strategi.....:'
080900000613     O                       vlrhdl              +1
081000000616     O          E            findett     2
081100000613     O                                              'Giusti...................:'
081200000613     O                       §CTROK        2   +  1
081300000616     O          E            findett     2
081400000613     O                                              'Sbagliati e corretti.....:'
081500000613     O                       §CTRMO        2   +  1
081600000616     O          E            findett     2
081700000613     O                                              'Sbagliati e scartati.....:'
081800000613     O                       §CTRNO        2   +  1
081900000512** CMD - COMANDI CL
082000110502OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2E9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
082100110502OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2E9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
082200110502DLTOVR FILE(TITV2E9P TITV2E9PS) LVL(*)
082300000512
082400000512
