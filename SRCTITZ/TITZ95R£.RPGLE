000100130503      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
000400030624     Ffnacr01l  if   e           k disk
000500090617     Ftntbe01l  if   e           k disk
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
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500050826     D fnvapds       e ds                  extname(fnvap00f)
002600071016     D tisi95ds      e ds
002700011105     D dorm01        e ds
002800011119     D dfar          e ds
002900110502     D trul33ds      e ds
003000110502     D kpjba         e ds
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600110502     D §numori         s              7  0 inz
003700011122     D*-------------------
003800010731     D* COSTANTI
003900011122     D*-------------------
004000010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004100010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004200990908
004300010528
004400010528
004500010528
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000724     C*
005000010528     C                   exsr      opeini
005100010528     C                   exsr      rwvao
005200110701     C*
005300110701     C* Esegue lancio TISI95R solo x chiusura
005400110701     C                   CLEAR                   TISI95DS
005500110701     C                   EVAL      I95TLA = 'C'
005600110701     C                   CALL      'TISI95R'
005700110701     C                   PARM                    TISI95DS
005800010528     C*
005900010528     C                   seton                                        lr
006000010528
006100010528
006200010528
006300010528
006400010528     C*--------------------------------------------------------
006500010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006600010528     C*--------------------------------------------------------
006700010528     C     PREELA        BEGSR
006800010528     C*
006900000724     C* SE OCCORRE SPEDIRE IN FILIALE
007000010528     C                   if        invfil <> *zeros and
007100010528     C                             flgGiro = '0'
007200010528     C*
007300010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007400010528     C                   eval      flgGiro = '1'
007500010529     C*
007600010529     C                   endif
007700010528     C*
007800010528     C                   ENDSR
007900010528     C***
008000010528
008100010528
008200010528     C*--------------------------------------------------------
008300010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008400010528     C*--------------------------------------------------------
008500010528     C     ENDELA        BEGSR
008600000613     C*
008700000613     C*
008800010528     C                   ENDSR
008900010528
009000010528
009100910830     C*--------------------------------------------------------
009200020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
009300910830     C*--------------------------------------------------------
009400010122     C     RWVAO         BEGSR
009500010528     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900010122     C                   if        not %open(fnvaowwr)
010000010122     C                   open      fnvaowwr
010100990914     C                   endif
010200990910     C*
010300990910     C                   clear                   §CTROK
010400990910     C                   clear                   §CTRMO
010500990910     C                   clear                   §CTRNO
010600990910     C*
010700921023     C                   DO        *HIVAL
010800990913     C*
010900990915     C                   READ      tivin00r                               70
011000010731     C*
011100010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011200010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
011300010618     C*
011400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011500010618     C                   if        vinflg = '1'
011600010618     C                   eval      flgOk = '1'
011700010618     C                   endif
011800010618     C*
011900000905     C                   if        vindta > *blanks
012000000613     C                   add       1             rrnum
012100990913     C*
012200010711     C                   if        *in70 = *off and
012300010711     C                             (vinflg = *blanks
012400010711     C                              or vinflg = '0'
012500010711     C                              or vinflg = '2')
012600990913     C*
012700010528     C                   clear                   FNVAO000
012800011105     C                   clear                   TIORI000
012900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013000010711     C                   if        vinflg = *blanks or vinflg = '0'
013100010711     C                   clear                   vinmsg
013200010711     C                   endif
013300020507     C*
013400020507     C                   exsr      inzvar
013500020527     C                   exsr      defcam
013600020507     C                   exsr      impvao
013700010122     C*
013800010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013900010528     C                   eval      depfil = VAOPOE
014000010528     C                   exsr      repfil
014100020507     C                   if        depfil = invfil
014200010528     C*
014300010528     C                   exsr      PREELA
014400010710     C*
014500930409     C*
014600010604     C  N31              ADD       1             §CTROK            7 0
014700010604     C   32              ADD       1             §CTRMO            7 0
014800010604     C   31              ADD       1             §CTRNO            7 0
014900020507     C*
015000030624     C  N31              EXSR      CHKCOR
015100011105     C  N31              EXSR      WRIORI
015200110503     C* N31              EXSR      WRIVGD
015300010122     C  N31              WRITE     FNVAO000
015400990910     C*
015500010604     C                   if        *in31 = *off and
015600010604     C                             *in32 = *off
015700990910     C                   eval      vinflg = '1'
015800990910     C                   else
015900990910     C                   eval      vinflg = '2'
016000991022     C                   endif
016100990910     C                   endif
016200010604     C*
016300010604     C                   endif
016400000905     C*
016500000905     C                   else
016600000905     C                   eval      vinflg = '1'
016700000905     C                   endif
016800000905     C*
016900000905     C  N70              update    tivin000
017000991022     C*
017100991022     C  N70              ENDdo
017200010528     C*
017300010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017400010528     C                   if        cntNonEl = *zeros or
017500010528     C                             flgMulti = '0'
017600010528     C* Se non ci sono record con errori ...
017700010528     C                   if        §ctrno = 0 and
017800010604     C                             §ctrmo = 0 and
017900010528     C                             flgStato <> '2'
018000990910     C* ... restituisco esito OK.
018100990921     C                   eval      wrkesito = '0'
018200990910     C                   else
018300000710     C                   if        §ctrok > 0
018400990921     C                   eval      wrkesito = '1'
018500000710     C                   else
018600010615     C                   if        flgOk = '0'
018700010615     C                   eval      wrkesito = '2'
018800010615     C                   else
018900010615     C                   eval      wrkesito = '6'
019000010615     C                   endif
019100990910     C                   endif
019200010528     C                   endif
019300010529     C                   else
019400010529     C                   eval      wrkesito = '9'
019500000710     C                   endif
019600990910     C*
019700990914     C                   if        %open(tivin00r)
019800990908     C                   close     tivin00r
019900990914     C                   endif
020000010122     C                   if        %open(fnvaowwr)
020100010122     C                   close     fnvaowwr
020200990914     C                   endif
020300990910     C*
020400010528     C                   if        vlrpoi <> 999
020500010528     C                   eval      invfil = vlrpoi
020600010528     C                   endif
020700010528     C*
020800990920     C                   if        §ctrok > 0
020900010528     C                             and invfil > *zeros
021000000613     C                   exsr      invio
021100990920     C                   endif
021200010612     C*
021300010612     C                   if        flgGiro = '1'
021400010612     C                   exsr      endela
021500010612     C                   endif
021600010528     C*
021700910830     C                   ENDSR
021800000613     C***
021900990920
022000020507
022100020507     C*----------------------------------------------------*
022200020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022300020507     C*----------------------------------------------------*
022400020507     C     INZVAR        BEGSR
022500020507     C*
022600020507     C                   Z-ADD     *zeros        Num5_0            5 0
022700020507     C                   Z-ADD     *zeros        Num3_0            3 0
022800020507     C*
022900020507     C                   ENDSR
023000020507     C*----------------------------------------------------*
023100020507     C*  IMPOSTAZIONE CAMPI COSTANTI
023200020507     C*----------------------------------------------------*
023300020507     C     DEFCAM        BEGSR
023400020507     C*
023500020507     C* Imposto i valori di default...
023600130626     C                   EVAL      VAOPOE = 210
023700130503     C                   EVAL      VAOTOR = 'S'
023800130503     C                   EVAL      VAOTCO = 'F'
023900130626     C                   EVAL      VAONAR = *blank
024000130626     C                   EVAL      VAOORR = 1400
024100130626     C                   EVAL      VAORMP = 'P'
024200130626     C                   EVAL      VAOPAG = 'O'
024300130626     C                   EVAL      VAONAC = *blank
024400020507     C* ... e poi verifico se sono stati passati come parametri
024500020507     C                   IF        vlrppt > *blanks
024600020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
024700020507     C                   EXSR      CHKNUM
024800020507     C                   IF        PiInt=*on
024900020507     C                   Z-ADD     PiVal         VAOCOR
025000020507     C                   Z-ADD     PiVal         VAOCRC
025100110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
025200020507     C                   ENDIF
025300020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025400020507     C                   EXSR      CHKNUM
025500020507     C                   IF        PiInt=*on
025600020507     C                   Z-ADD     PiVal         VAOPOE
025700020507     C                   Z-ADD     PiVal         VAOPOC
025800020507     C                   ENDIF
025900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026000020507     C                   EXSR      CHKNUM
026100020507     C                   IF        PiInt=*on
026200020507     C                   Z-ADD     PiVal         Num3_0
026300020507     C                   MOVEL     NUM3_0        VAOCTR
026400020507     C                   ENDIF
026500130626
026600130626     C* se il 17° char dei parametri è T=Test
026700130626     C                   SETOFF                                       55
026800130626     C                   IF        %subst(vlrppt:17:1) = 'T'
026900130626     C* inibisco la scrittura dei file TIORI e TIVGD
027000130626     C                   SETON                                        55
027100130626     C* imposto il PO di emissione come filiale fittizia
027200130626     C                   EVAL      VAOPOE = 999
027300130626     C                   ENDIF
027400130626
027500020507     C                   ENDIF
027600020507     C*
027700020507     C                   ENDSR
027800020507     C*----------------------------------------------------*
027900020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
028000020507     C*----------------------------------------------------*
028100020507     C     IMPVAO        BEGSR
028200020507     C*
028300020507     C                   SETOFF                                       3132
028400020507     C*
028500020507     C* Reperimento campi ALFA
028600020507     C*
028700110701     C* Dati x il ritiro
028800110803     C                   EVAL      VAORSR=%trim(%subst(vindta:219:40))
028900020507     C* == verifico se esiste il carattere @e, se c'è diventa A
029000020507     C     '@':'A'       XLATE     VAORSR        VAORSR
029100020507     C* ==
029200110803     C                   EVAL      VAOINR=%trim(%subst(vindta:259:35))
029300110803     C                   EVAL      VAOCAR=%trim(%subst(vindta:294:5))
029400110803     C                   EVAL      VAOLOR=%trim(%subst(vindta:299:40))
029500110803     C                   EVAL      VAOPRR=%trim(%subst(vindta:339:2))
029600110803     C                   EVAL      VAOTER=%trim(%subst(vindta:341:15))
029700111223     C                   EVAL      VAORER=%trim(%subst(vindta:356:20))
029800110701     C* Reperisco la provincia dal CAP e dalla località
029900110701     C                   IF        VAOCAR <> *blanks AND
030000110701     C                             VAOPRR  = *blanks AND
030100110701     C                             VAONAR  = *blanks
030200110701     C                   CLEAR                   TISI95DS
030300110701     C                   EVAL      I95TCN = '3'
030400110701     C                   Z-ADD     datcor        I95DAT
030500110701     C                   EVAL      I95CAP = VAOCAR
030600110701     C                   EVAL      I95LOC = VAOLOR
030700110701     C                   CALL      'TISI95R'
030800110701     C                   PARM                    TISI95DS
030900110701     C                   EVAL      VAOPRR = O95PRV
031000110701     C                   ENDIF
031100110502     C*
031200110701     C* Dati x la consegna
031300110803     C                   EVAL      VAORSC=%trim(%subst(vindta:506:40))
031400110502     C* == verifico se esiste il carattere @e, se c'è diventa A
031500110502     C     '@':'A'       XLATE     VAORSC        VAORSC
031600110502     C* ==
031700110803     C                   EVAL      VAOINC=%trim(%subst(vindta:546:35))
031800110803     C                   EVAL      VAOCAC=%trim(%subst(vindta:581:5))
031900110803     C                   EVAL      VAOLOC=%trim(%subst(vindta:586:40))
032000110803     C                   EVAL      VAOPRC=%trim(%subst(vindta:626:2))
032100130626     C*
032200110701     C* Dati genericio ORM
032300110803     C                   EVAL      VAORFA=%trim(%subst(vindta:745:10))
032400111223     C                   EVAL      VAONO1=%trim(%subst(vindta:426:35))
032500111223     C                   EVAL      VAONO2=%trim(%subst(vindta:426+35:35))
032600111223
032700111223      * Natura merce
032800111223     C                   SELECT
032900130626     c                   WHEN      %subst(vindta:706:1) = 'S'
033000130626     C                   EVAL      VAONAM='BUSTE'
033100130626     c                   WHEN      %subst(vindta:706:1) = 'C'
033200130626     C                   EVAL      VAONAM='COLLI'
033300130626     c                   WHEN      %subst(vindta:706:1) = 'B'
033400130626     C                   EVAL      VAONAM='BAULETTI PICCOLI'
033500130626     c                   WHEN      %subst(vindta:706:1) = 'D'
033600130626     C                   EVAL      VAONAM='BAULETTI GRANDI'
033700111223     c                   ENDSL
033800110803     C*
033900111223      * Fermo deposito
034000110803     C                   SELECT
034100131009     C                   WHEN      %subst(vindta:709:1) = '1'
034200130626     C                   EVAL      VAOFFD='S'
034300110803     C                   ENDSL
034400020507     C*
034500020507     C* Reperimento campi NUMERICI
034600020507     C*
034700020527     C* Data e ora apertura chiamata
034800020527     C                   Z-ADD     datcor        VAODAO
034900071016     C                   MOVEL     wn14          VAOOAO
035000020507     C*
035100130626     C* Codice cliente pagante e ordinante
035200130626     C                   EVAL      PiStr=%subst(vindta:1:11)
035300020507     C                   EXSR      CHKNUM
035400020507     C                   IF        PiInt=*on
035500130626     C                   Z-ADD     PiVal         VAOKSC
035600130626     C                   EVAL      VAOCOR = VAOKSC * 1000
035700020507     C                   ELSE
035800020507     C                   SETON                                        32
035900130626     C                   EVAL      VAOKSC = *zeros
036000020507     C                   EVAL      vinmsg = %trimr(vinmsg)
036100130626     C                             + ' ' + 'VAOKSC'
036200020507     C                   ENDIF
036300130626     C*
036400130626     C* Data ritiro richiesta
036500130626     C                   EVAL      PiStr=%subst(vindta:682:4)+
036600130626     C                                   %subst(vindta:680:2)+
036700130626     C                                   %subst(vindta:678:2)
036800130626     C                   EXSR      CHKNUM
036900130626     C                   IF        PiInt=*on
037000130626     C                   Z-ADD     PiVal         VAODAR
037100130626     C                   ELSE
037200130626     C                   SETON                                        32
037300130626     C                   EVAL      VAODAR = *zeros
037400130626     C                   EVAL      vinmsg = %trimr(vinmsg)
037500130626     C                             + ' ' + 'VAODAR'
037600130626     C                   ENDIF
037700130626     C*
037800130626     C* Ora ritiro richiesta
037900130626     C                   EVAL      PiStr=%subst(vindta:702:4)
038000130626     C                   EXSR      CHKNUM
038100130626     C                   IF        PiInt=*on
038200130626     C                   Z-ADD     PiVal         VAOORR
038300130626     C                   ELSE
038400130626     C                   SETON                                        32
038500130626     C                   EVAL      VAOORR = *zeros
038600130626     C                   EVAL      vinmsg = %trimr(vinmsg)
038700130626     C                             + ' ' + 'VAOORR'
038800130626     C                   ENDIF
038900071012     C*
039000071012     C* Colli
039100110803     C                   EVAL      PiStr=%subst(vindta:723:5)
039200071012     C                   EXSR      CHKNUM
039300071012     C                   IF        PiInt=*on
039400071012     C                   Z-ADD     PiVal         VAONCL
039500071012     C                   ELSE
039600071012     C                   SETON                                        32
039700071012     C                   EVAL      VAONCL = *zeros
039800071012     C                   EVAL      vinmsg = %trimr(vinmsg)
039900071012     C                             + ' ' + 'VAONCL'
040000071012     C                   ENDIF
040100071012     C*
040200071012     C* Peso Kg
040300130626     C                   EVAL      PiStr=%subst(vindta:728:8)
040400071012     C                   EXSR      CHKNUM
040500071012     C                   IF        PiNum=*on
040600130626     C                   EVAL      VAOPKG = PiVal/100
040700071012     C                   ELSE
040800071012     C                   SETON                                        32
040900071012     C                   EVAL      VAOPKG = *zeros
041000071012     C                   EVAL      vinmsg = %trimr(vinmsg)
041100071012     C                             + ' ' + 'VAOPKG'
041200071012     C                   ENDIF
041300111223
041400111223      * forzo 1 collo se = 0
041500111223     c                   IF        VAOncl = 0
041600111223     c                   eval      VAOncl = 1
041700111223     c                   ENDIF
041800111223
041900020507     C*
042000020507     C                   ENDSR
042100020507     C*----------------------------------------------------*
042200020507
042300020507
042400020507
042500020507     C*----------------------------------------------------*
042600020507     C*  CONTROLLO NUMERICITA' CAMPI
042700020507     C*----------------------------------------------------*
042800020507     C     CHKNUM        BEGSR
042900020507     C*
043000020507     C                   IF        PiDecChr = *blanks
043100020507     C                   EVAL      PiDecChr = '.'
043200020507     C                   ENDIF
043300020507     C*
043400020507     C                   CALL(e)   'ISNUMERIC'
043500020507     C                   PARM                    PiStr            30
043600020507     C                   PARM                    PiDecChr          1
043700020507     C                   PARM      *ZEROS        PiVal            30 9
043800020507     C                   PARM      '0'           PiInt             1
043900020507     C                   PARM      '0'           PiNum             1
044000020507     C                   IF        %error
044100020507     C                   EVAL      PiNum=*off
044200020507     C                   ENDIF
044300020507     C*
044400020507     C                   ENDSR
044500020507     C***
044600030624
044700030624
044800030624     C*------------------------------------------------------------------------*
044900030624     C* CHKCOR - Considerazioni su VAOCOR
045000030624     C*------------------------------------------------------------------------*
045100030624     C     CHKCOR        BEGSR
045200030624     C*
045300030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
045400030624     C                   if        vaoTCO = 'F'
045500030624     C* Quindi verifico se il codice ordinante non è valorizzato
045600030625     C                   if        vaoCOR = *zeros  AND
045700030625     C                             vaoRSO = *blanks AND
045800030625     C                             vaoINO = *blanks AND
045900030625     C                             vaoCAO = *blanks AND
046000030625     C                             vaoLOO = *blanks AND
046100030625     C                             vaoPRO = *blanks AND
046200030625     C                             vaoNAO = *blanks
046300030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
046400030624     C                   movel     '0'           wFlgAcr           1
046500030624     C                   move      vlrKSC        wCli              7 0
046600030624     C                   move(p)   wCli          acrCRO
046700030624     C     acrCRO        setll     fnacr01l
046800030624     C                   if        %found(fnacr01l)
046900030624     C                   read      fnacr01l
047000030624     C                   dow       not %eof(fnacr01l)
047100030624     C                   movel     acrCRO        wCliAcr           7 0
047200030624     C                   if        wCliAcr = wCli
047300030624     C                   eval      vaoCOR = acrCRO
047400030624     C                   movel     '1'           wFlgAcr
047500030624     C                   leave
047600030624     C                   else
047700030624     C                   leave
047800030624     C                   endif
047900030624     C                   read      fnacr01l
048000030624     C                   enddo
048100030624     C                   endif
048200030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
048300030624     C* dati anagrafici dell'ordinante dal piano dei conti
048400030624     C                   if        wFlgAcr = '0'
048500030624     C                   clear                   BS69DS
048600030624     C                   clear                   ACODS
048700030624     C                   clear                   INDDS
048800030624     C                   clear                   CLPDS
048900030624     C                   clear                   CLSDS
049000030624     C                   move(P)   vlrKSC        I69KAC
049100030624     C                   move(P)   vlrKSC        I69KIN
049200030624     C                   call      'TIBS69R'
049300030624     C                   parm                    BS69DS
049400030624     C                   parm                    ACODS
049500030624     C                   parm                    INDDS
049600030624     C                   parm                    CLPDS
049700030624     C                   parm                    CLSDS
049800030624     C     O69ERR        ifne      '1'
049900030624     C                   eval      vaoRSO = ACORAG
050000030624     C                   eval      vaoINO = INDVIA
050100030624     C                   movel(P)  INDCAP        vaoCAO
050200030624     C                   eval      vaoLOO = INDCIT
050300030624     C                   eval      vaoPRO = INDPRV
050400030624     C                   eval      vaoNAO = INDSTA
050500030624     C                   endif
050600030624     C                   endif
050700030624     C                   endif
050800030624     C                   endif
050900030624     C*
051000030624     C                   ENDSR
051100020507
051200020507
051300010528
051400011105     C*------------------------------------------------------------------------*
051500011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
051600011105     C*------------------------------------------------------------------------*
051700011105     C     WRIORI        BEGSR
051800011105     C*
051900090617     C* Come prima cosa stacco un numeratore da AZNUM
052000090617     C                   clear                   trul33ds
052100090617     C                   eval      I33OPE = *zeros
052200090617     C                   eval      I33CNU = 600
052300090617     C                   eval      I33NUM = 1
052400090617     C                   movel     TRUL33DS      KPJBU
052500090617     C                   call      'TRUL33R'
052600090617     C                   parm                    KPJBA
052700090617     C                   movel     KPJBU         TRUL33DS
052800090617     C                   if        O33ERR = *zeros
052900090617     C                   z-add     O33NRF        §numori
053000090618     c                   else
053100090618     c                   eval      §numori = *all'9'
053200090617     C                   endif
053300011105     C*
053400110502     C* Quindi imposto i campi "extra-VAO"
053500011105     C                   eval      oriPRG = §numori
053600011105     C                   eval      oriDER = datcor
053700110502     C                   eval      oriOER = oracor
053800011105     C                   eval      oriPOE = vaoPOE
053900011105     C                   eval      oriNSR = vaoNSR
054000011105     C                   eval      oriNOR = vaoNOR
054100011105     C                   eval      oriNRV = vaoNRV
054200011105     C                   eval      oriFLO = *blanks
054300011105     C*
054400011122     C* Infine valorizzo la chiave esterna sul file FNVAO
054500011105     C                   eval      dorm01 = vaoFLO
054600011105     C                   movel     §numori       §ormpg
054700110502     C                   movel     'S'           §ormfr
054800111223     C                   movel     'S'           §ormks
054900111223     C                   movel     'S'           §ormfd
055000130626     C                   movel     'N'           §orcomc
055100011105     C                   eval      vaoFLO = dorm01
055200030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
055300030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
055400011105     C*
055500011105     C* ...quindi scrivo il file TIORI00F
055600130626     C  N55              WRITE     TIORI000
055700011105     C*
055800011105     C                   ENDSR
055900020507     C***
056000050826
056100050826
056200050826
056300050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
056400050826     C     wriVGD        BEGSR
056500050826     C*
056600050826     C* Reperisco la descrizione della fase da tabella
056700050826     C                   movel(p)  'FAR'         KteCOD
056800050826     C                   movel(p)  '000'         KteKE1
056900050826     C     KEYtbe        chain     tntbe01l
057000050826     C                   if        %found(tntbe01l)
057100050826     C                   movel(P)  tbeuni        DFAR
057200050826     C                   else
057300050826     C                   clear                   DFAR
057400050826     C                   endif
057500050826     C*
057600050826     C* Valorizzo buffer record
057700050826     C                   clear                   FNVAPDS
057800050826     C                   eval      vapIDC = oriIDC
057900050826     C                   eval      vapPOE = oriPOE
058000050826     C                   eval      vapRFA = vaoRFA
058100050826     C                   eval      vapPOG = oriPOE
058200050826     C                   eval      vapDAE = oriDER
058300050826     C                   eval      vapORE = oriOER
058400050826     C                   if        d§fardva <> *blanks
058500050826     C                   eval      vapDFA = d§fardva
058600050826     C                   else
058700050826     C                   eval      vapDFA = d§fardes
058800050826     C                   endif
058900050826     C*
059000050826     C                   clear                   tivgd000
059100080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
059200050826     C                   eval      vgdTIP = 'VP'
059300050826     C                   eval      vgdKSU = vapIDC
059400050826     C                   eval      vgdTSC = 'WW'
059500050826     C                   eval      vgdDAT = datcor
059600111223     C                   eval      vgdPGM = 'TITZ04R'
059700130626     C  N55              write     tivgd000
059800050826     C*
059900050826     C                   ENDSR
060000050826     C*------------------------------------------------------------------------*
060100011119
060200010528
060300010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
060400010528     C     repfil        BEGSR
060500010528     C*
060600010528     C                   if        invfil = *zeros and
060700010528     C                             depfil > *zeros and
060800010613     C                             (vinflg = *blanks or
060900010613     C                              vinflg = *zeros)
061000010528     C
061100010528     C                   eval      invfil = depfil
061200010528     C                   endif
061300010528     C*
061400010529     C                   if        depfil <> invfil and
061500010529     C                             invfil > *zeros
061600010528     C                   eval      flgMulti = '1'
061700010528     C                   if        vinflg = *blanks
061800010528     C                   add       1             cntNonEl
061900010528     C                   endif
062000010528     C                   endif
062100010528     C*
062200010528     C                   if        vinflg = '2'
062300010528     C                   eval      flgStato = '2'
062400010528     C                   endif
062500010528     C*
062600010528     C                   ENDSR
062700010528     C***
062800010528
062900010528
063000010528
063100010528
063200990920      /TITLE Invio dei dati al punto operativo.
063300000613     C     invio         BEGSR
063400990920     C*
063500990920     C                   reset                   dscmz
063600010528     C                   move      invfil        cmzdst
063700990920     C                   eval      cmzfld = vlrfou
063800990920     C                   eval      cmzmbd = vlrhdl
063900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
064000000710     C                   if        prmfir = *blanks
064100010122     C                   eval      cmzfla = 'FNVAO00F'
064200010122     C                   eval      cmzmba = 'FNVAO00F'
064300000710     C                   else
064400000710     C                   eval      cmzfla = prmfir
064500000710     C                   eval      cmzmba = prmfir
064600000710     C                   endif
064700990920     C                   eval      cmznrr = *zeros
064800990920     C                   move      §ctrok        cmznrr
064900010123     C                   eval      cmzlba = vlrfl1
065000990920     C                   call(e)   'TIS711C'
065100990920     C                   parm                    dscmz
065200990921     C                   parm      *blanks       esito
065300990920     C                   if        %error
065400990920     C                             or cmzerr = '1'
065500990921     C                             or esito  = '1'
065600000710     C                   eval      wrkesito = '3'
065700990920     C                   endif
065800990920     C*
065900000613     C                   ENDSR
066000000613     C***
066100990910
066200010528
066300010528
066400010528
066500010528      /TITLE Invio dei dati al punto operativo.
066600010528     C     opeini        BEGSR
066700010528     C*
066800010528     C* Inizializzo flag e contatori operativi
066900010528     C                   movel     '0'           flgGiro           1
067000010528     C                   movel     '0'           flgMulti          1
067100010528     C                   movel     '1'           flgStato          1
067200010615     C                   movel     '0'           flgOk             1
067300010528     C                   z-add     *zeros        cntNonEl         10 0
067400010528     C                   z-add     *zeros        depfil            3 0
067500010528     C                   z-add     *zeros        invfil            3 0
067600010528     C*
067700010528     C                   ENDSR
067800010528     C***
067900010528
068000010528
068100010528
068200010528
068300000613     C     *inzsr        BEGSR
068400990910     C*
068500990910     C     *entry        plist
068600990920     C                   parm                    tivlrds
068700990921     C                   parm      wrkesito      esito
068800000724     C                   parm                    prmlit
068900000710     C                   parm                    prmfir
069000011105     C*
069100011105     C* Campi riferito al database
069200011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
069300011105     C     *like         define    tbeke1        kteKE1
069400011105     C*
069500011105     C* Definizione chiavi di lettura
069600011105     C* TNTBE01L - parziale
069700011105     C     keytbe        klist
069800011105     C                   kfld                    kteCOD                         *UTENTE
069900011105     C                   kfld                    kteKE1                         *CODICE
070000011105     C*
070100011105     C* Reperimento data e ora corrente
070200110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
070300110502     C                   movel     wn14          oracor            6 0          *ORA (6)
070400110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070500110502     C                   eval      datcor = %dec(%date() : *ISO)
070600010529     C*
070700000613     C                   ENDSR
