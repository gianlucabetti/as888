000100111223      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente PRENATAL - cod. 1661332)
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
023600111223     C                   EVAL      VAOPOE = 166
023700020507     C                   EVAL      VAOTOR = 'S'
023800020507     C                   EVAL      VAOTCO = 'F'
023900111223     C                   EVAL      VAOCOR = 1661332000
024000110502     C                   EVAL      VAOPAG = 'O'
024100111223     C                   EVAL      VAOKSC = 1661332
024200111223     C                   EVAL      VAODDT = 'S'
024300111223     C                   EVAL      VAOCTR = '300'
024400110803     C                   EVAL      VAOORR = 1400
024500111223     C                   EVAL      ORIIDC = '01661332'
024600020507     C* ... e poi verifico se sono stati passati come parametri
024700020507     C                   IF        vlrppt > *blanks
024800020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
024900020507     C                   EXSR      CHKNUM
025000020507     C                   IF        PiInt=*on
025100020507     C                   Z-ADD     PiVal         VAOCOR
025200020507     C                   Z-ADD     PiVal         VAOCRC
025300110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
025400020507     C                   ENDIF
025500020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025600020507     C                   EXSR      CHKNUM
025700020507     C                   IF        PiInt=*on
025800020507     C                   Z-ADD     PiVal         VAOPOE
025900020507     C                   Z-ADD     PiVal         VAOPOC
026000020507     C                   ENDIF
026100020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026200020507     C                   EXSR      CHKNUM
026300020507     C                   IF        PiInt=*on
026400020507     C                   Z-ADD     PiVal         Num3_0
026500020507     C                   MOVEL     NUM3_0        VAOCTR
026600020507     C                   ENDIF
026700020507     C                   ENDIF
026800020507     C*
026900020507     C                   ENDSR
027000020507     C*----------------------------------------------------*
027100020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
027200020507     C*----------------------------------------------------*
027300020507     C     IMPVAO        BEGSR
027400020507     C*
027500020507     C                   SETOFF                                       3132
027600020507     C*
027700020507     C* Reperimento campi ALFA
027800020507     C*
027900110701     C* Dati x il ritiro
028000110803     C                   EVAL      VAORSR=%trim(%subst(vindta:219:40))
028100020507     C* == verifico se esiste il carattere @e, se c'è diventa A
028200020507     C     '@':'A'       XLATE     VAORSR        VAORSR
028300020507     C* ==
028400110803     C                   EVAL      VAOINR=%trim(%subst(vindta:259:35))
028500110803     C                   EVAL      VAOCAR=%trim(%subst(vindta:294:5))
028600110803     C                   EVAL      VAOLOR=%trim(%subst(vindta:299:40))
028700110803     C                   EVAL      VAOPRR=%trim(%subst(vindta:339:2))
028800110803     C                   EVAL      VAOTER=%trim(%subst(vindta:341:15))
028900111223     C                   EVAL      VAORER=%trim(%subst(vindta:356:20))
029000110701     C* Reperisco la provincia dal CAP e dalla località
029100110701     C                   IF        VAOCAR <> *blanks AND
029200110701     C                             VAOPRR  = *blanks AND
029300110701     C                             VAONAR  = *blanks
029400110701     C                   CLEAR                   TISI95DS
029500110701     C                   EVAL      I95TCN = '3'
029600110701     C                   Z-ADD     datcor        I95DAT
029700110701     C                   EVAL      I95CAP = VAOCAR
029800110701     C                   EVAL      I95LOC = VAOLOR
029900110701     C                   CALL      'TISI95R'
030000110701     C                   PARM                    TISI95DS
030100110701     C                   EVAL      VAOPRR = O95PRV
030200110701     C                   ENDIF
030300110502     C*
030400110701     C* Dati x la consegna
030500110803     C                   EVAL      VAORSC=%trim(%subst(vindta:506:40))
030600110502     C* == verifico se esiste il carattere @e, se c'è diventa A
030700110502     C     '@':'A'       XLATE     VAORSC        VAORSC
030800110502     C* ==
030900110803     C                   EVAL      VAOINC=%trim(%subst(vindta:546:35))
031000110803     C                   EVAL      VAOCAC=%trim(%subst(vindta:581:5))
031100110803     C                   EVAL      VAOLOC=%trim(%subst(vindta:586:40))
031200110803     C                   EVAL      VAOPRC=%trim(%subst(vindta:626:2))
031300110502     C*
031400110701     C* Dati genericio ORM
031500110803     C                   EVAL      VAORFA=%trim(%subst(vindta:745:10))
031600111223     C                   EVAL      VAONO1=%trim(%subst(vindta:426:35))
031700111223     C                   EVAL      VAONO2=%trim(%subst(vindta:426+35:35))
031800111223
031900111223      * Natura merce
032000111223     C                   SELECT
032100111223     c                   WHEN      %subst(vindta:706:1) = 'C'
032200111223     C                   EVAL      VAONAM='COLLI'
032300111223     c                   ENDSL
032400110803     C*
032500111223      * Fermo deposito
032600110803     C                   SELECT
032700110803     C                   WHEN      %subst(vindta:709:1) = '1'
032800110803     C                   EVAL      VAOFFD='S'
032900110803     C                   ENDSL
033000020507     C*
033100020507     C* Reperimento campi NUMERICI
033200020507     C*
033300020527     C* Data e ora apertura chiamata
033400020527     C                   Z-ADD     datcor        VAODAO
033500071016     C                   MOVEL     wn14          VAOOAO
033600020507     C*
033700020507     C* Data ritiro richiesta
033800110803     C                   EVAL      PiStr=%subst(vindta:682:4)+
033900110803     C                                   %subst(vindta:680:2)+
034000110803     C                                   %subst(vindta:678:2)
034100020507     C                   EXSR      CHKNUM
034200020507     C                   IF        PiInt=*on
034300020507     C                   Z-ADD     PiVal         VAODAR
034400020507     C                   ELSE
034500020507     C                   SETON                                        32
034600020507     C                   EVAL      VAODAR = *zeros
034700020507     C                   EVAL      vinmsg = %trimr(vinmsg)
034800020507     C                             + ' ' + 'VAODAR'
034900020507     C                   ENDIF
035000071012     C*
035100071012     C* Colli
035200110803     C                   EVAL      PiStr=%subst(vindta:723:5)
035300071012     C                   EXSR      CHKNUM
035400071012     C                   IF        PiInt=*on
035500071012     C                   Z-ADD     PiVal         VAONCL
035600071012     C                   ELSE
035700071012     C                   SETON                                        32
035800071012     C                   EVAL      VAONCL = *zeros
035900071012     C                   EVAL      vinmsg = %trimr(vinmsg)
036000071012     C                             + ' ' + 'VAONCL'
036100071012     C                   ENDIF
036200071012     C*
036300071012     C* Peso Kg
036400111223     C                   EVAL      PiStr=%subst(vindta:728:4)
036500071012     C                   EXSR      CHKNUM
036600071012     C                   IF        PiNum=*on
036700071012     C                   Z-ADD     PiVal         VAOPKG
036800071012     C                   ELSE
036900071012     C                   SETON                                        32
037000071012     C                   EVAL      VAOPKG = *zeros
037100071012     C                   EVAL      vinmsg = %trimr(vinmsg)
037200071012     C                             + ' ' + 'VAOPKG'
037300071012     C                   ENDIF
037400111223
037500111223      * forzo 1 collo se = 0
037600111223     c                   IF        VAOncl = 0
037700111223     c                   eval      VAOncl = 1
037800111223     c                   ENDIF
037900111223
038000020507     C*
038100020507     C                   ENDSR
038200020507     C*----------------------------------------------------*
038300020507
038400020507
038500020507
038600020507     C*----------------------------------------------------*
038700020507     C*  CONTROLLO NUMERICITA' CAMPI
038800020507     C*----------------------------------------------------*
038900020507     C     CHKNUM        BEGSR
039000020507     C*
039100020507     C                   IF        PiDecChr = *blanks
039200020507     C                   EVAL      PiDecChr = '.'
039300020507     C                   ENDIF
039400020507     C*
039500020507     C                   CALL(e)   'ISNUMERIC'
039600020507     C                   PARM                    PiStr            30
039700020507     C                   PARM                    PiDecChr          1
039800020507     C                   PARM      *ZEROS        PiVal            30 9
039900020507     C                   PARM      '0'           PiInt             1
040000020507     C                   PARM      '0'           PiNum             1
040100020507     C                   IF        %error
040200020507     C                   EVAL      PiNum=*off
040300020507     C                   ENDIF
040400020507     C*
040500020507     C                   ENDSR
040600020507     C***
040700030624
040800030624
040900030624     C*------------------------------------------------------------------------*
041000030624     C* CHKCOR - Considerazioni su VAOCOR
041100030624     C*------------------------------------------------------------------------*
041200030624     C     CHKCOR        BEGSR
041300030624     C*
041400030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
041500030624     C                   if        vaoTCO = 'F'
041600030624     C* Quindi verifico se il codice ordinante non è valorizzato
041700030625     C                   if        vaoCOR = *zeros  AND
041800030625     C                             vaoRSO = *blanks AND
041900030625     C                             vaoINO = *blanks AND
042000030625     C                             vaoCAO = *blanks AND
042100030625     C                             vaoLOO = *blanks AND
042200030625     C                             vaoPRO = *blanks AND
042300030625     C                             vaoNAO = *blanks
042400030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
042500030624     C                   movel     '0'           wFlgAcr           1
042600030624     C                   move      vlrKSC        wCli              7 0
042700030624     C                   move(p)   wCli          acrCRO
042800030624     C     acrCRO        setll     fnacr01l
042900030624     C                   if        %found(fnacr01l)
043000030624     C                   read      fnacr01l
043100030624     C                   dow       not %eof(fnacr01l)
043200030624     C                   movel     acrCRO        wCliAcr           7 0
043300030624     C                   if        wCliAcr = wCli
043400030624     C                   eval      vaoCOR = acrCRO
043500030624     C                   movel     '1'           wFlgAcr
043600030624     C                   leave
043700030624     C                   else
043800030624     C                   leave
043900030624     C                   endif
044000030624     C                   read      fnacr01l
044100030624     C                   enddo
044200030624     C                   endif
044300030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
044400030624     C* dati anagrafici dell'ordinante dal piano dei conti
044500030624     C                   if        wFlgAcr = '0'
044600030624     C                   clear                   BS69DS
044700030624     C                   clear                   ACODS
044800030624     C                   clear                   INDDS
044900030624     C                   clear                   CLPDS
045000030624     C                   clear                   CLSDS
045100030624     C                   move(P)   vlrKSC        I69KAC
045200030624     C                   move(P)   vlrKSC        I69KIN
045300030624     C                   call      'TIBS69R'
045400030624     C                   parm                    BS69DS
045500030624     C                   parm                    ACODS
045600030624     C                   parm                    INDDS
045700030624     C                   parm                    CLPDS
045800030624     C                   parm                    CLSDS
045900030624     C     O69ERR        ifne      '1'
046000030624     C                   eval      vaoRSO = ACORAG
046100030624     C                   eval      vaoINO = INDVIA
046200030624     C                   movel(P)  INDCAP        vaoCAO
046300030624     C                   eval      vaoLOO = INDCIT
046400030624     C                   eval      vaoPRO = INDPRV
046500030624     C                   eval      vaoNAO = INDSTA
046600030624     C                   endif
046700030624     C                   endif
046800030624     C                   endif
046900030624     C                   endif
047000030624     C*
047100030624     C                   ENDSR
047200020507
047300020507
047400010528
047500011105     C*------------------------------------------------------------------------*
047600011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
047700011105     C*------------------------------------------------------------------------*
047800011105     C     WRIORI        BEGSR
047900011105     C*
048000090617     C* Come prima cosa stacco un numeratore da AZNUM
048100090617     C                   clear                   trul33ds
048200090617     C                   eval      I33OPE = *zeros
048300090617     C                   eval      I33CNU = 600
048400090617     C                   eval      I33NUM = 1
048500090617     C                   movel     TRUL33DS      KPJBU
048600090617     C                   call      'TRUL33R'
048700090617     C                   parm                    KPJBA
048800090617     C                   movel     KPJBU         TRUL33DS
048900090617     C                   if        O33ERR = *zeros
049000090617     C                   z-add     O33NRF        §numori
049100090618     c                   else
049200090618     c                   eval      §numori = *all'9'
049300090617     C                   endif
049400011105     C*
049500110502     C* Quindi imposto i campi "extra-VAO"
049600011105     C                   eval      oriPRG = §numori
049700011105     C                   eval      oriDER = datcor
049800110502     C                   eval      oriOER = oracor
049900011105     C                   eval      oriPOE = vaoPOE
050000011105     C                   eval      oriNSR = vaoNSR
050100011105     C                   eval      oriNOR = vaoNOR
050200011105     C                   eval      oriNRV = vaoNRV
050300011105     C                   eval      oriFLO = *blanks
050400011105     C*
050500011122     C* Infine valorizzo la chiave esterna sul file FNVAO
050600011105     C                   eval      dorm01 = vaoFLO
050700011105     C                   movel     §numori       §ormpg
050800110502     C                   movel     'S'           §ormfr
050900111223     C                   movel     'S'           §ormks
051000111223     C                   movel     'S'           §ormfd
051100130306     C***                movel     'N'           §orcomc
051200011105     C                   eval      vaoFLO = dorm01
051300030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
051400030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
051500011105     C*
051600011105     C* ...quindi scrivo il file TIORI00F
051700011105     C                   WRITE     TIORI000
051800011105     C*
051900011105     C                   ENDSR
052000020507     C***
052100050826
052200050826
052300050826
052400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
052500050826     C     wriVGD        BEGSR
052600050826     C*
052700050826     C* Reperisco la descrizione della fase da tabella
052800050826     C                   movel(p)  'FAR'         KteCOD
052900050826     C                   movel(p)  '000'         KteKE1
053000050826     C     KEYtbe        chain     tntbe01l
053100050826     C                   if        %found(tntbe01l)
053200050826     C                   movel(P)  tbeuni        DFAR
053300050826     C                   else
053400050826     C                   clear                   DFAR
053500050826     C                   endif
053600050826     C*
053700050826     C* Valorizzo buffer record
053800050826     C                   clear                   FNVAPDS
053900050826     C                   eval      vapIDC = oriIDC
054000050826     C                   eval      vapPOE = oriPOE
054100050826     C                   eval      vapRFA = vaoRFA
054200050826     C                   eval      vapPOG = oriPOE
054300050826     C                   eval      vapDAE = oriDER
054400050826     C                   eval      vapORE = oriOER
054500050826     C                   if        d§fardva <> *blanks
054600050826     C                   eval      vapDFA = d§fardva
054700050826     C                   else
054800050826     C                   eval      vapDFA = d§fardes
054900050826     C                   endif
055000050826     C*
055100050826     C                   clear                   tivgd000
055200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
055300050826     C                   eval      vgdTIP = 'VP'
055400050826     C                   eval      vgdKSU = vapIDC
055500050826     C                   eval      vgdTSC = 'WW'
055600050826     C                   eval      vgdDAT = datcor
055700111223     C                   eval      vgdPGM = 'TITZ04R'
055800050826     C                   write     tivgd000
055900050826     C*
056000050826     C                   ENDSR
056100050826     C*------------------------------------------------------------------------*
056200011119
056300010528
056400010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056500010528     C     repfil        BEGSR
056600010528     C*
056700010528     C                   if        invfil = *zeros and
056800010528     C                             depfil > *zeros and
056900010613     C                             (vinflg = *blanks or
057000010613     C                              vinflg = *zeros)
057100010528     C
057200010528     C                   eval      invfil = depfil
057300010528     C                   endif
057400010528     C*
057500010529     C                   if        depfil <> invfil and
057600010529     C                             invfil > *zeros
057700010528     C                   eval      flgMulti = '1'
057800010528     C                   if        vinflg = *blanks
057900010528     C                   add       1             cntNonEl
058000010528     C                   endif
058100010528     C                   endif
058200010528     C*
058300010528     C                   if        vinflg = '2'
058400010528     C                   eval      flgStato = '2'
058500010528     C                   endif
058600010528     C*
058700010528     C                   ENDSR
058800010528     C***
058900010528
059000010528
059100010528
059200010528
059300990920      /TITLE Invio dei dati al punto operativo.
059400000613     C     invio         BEGSR
059500990920     C*
059600990920     C                   reset                   dscmz
059700010528     C                   move      invfil        cmzdst
059800990920     C                   eval      cmzfld = vlrfou
059900990920     C                   eval      cmzmbd = vlrhdl
060000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060100000710     C                   if        prmfir = *blanks
060200010122     C                   eval      cmzfla = 'FNVAO00F'
060300010122     C                   eval      cmzmba = 'FNVAO00F'
060400000710     C                   else
060500000710     C                   eval      cmzfla = prmfir
060600000710     C                   eval      cmzmba = prmfir
060700000710     C                   endif
060800990920     C                   eval      cmznrr = *zeros
060900990920     C                   move      §ctrok        cmznrr
061000010123     C                   eval      cmzlba = vlrfl1
061100990920     C                   call(e)   'TIS711C'
061200990920     C                   parm                    dscmz
061300990921     C                   parm      *blanks       esito
061400990920     C                   if        %error
061500990920     C                             or cmzerr = '1'
061600990921     C                             or esito  = '1'
061700000710     C                   eval      wrkesito = '3'
061800990920     C                   endif
061900990920     C*
062000000613     C                   ENDSR
062100000613     C***
062200990910
062300010528
062400010528
062500010528
062600010528      /TITLE Invio dei dati al punto operativo.
062700010528     C     opeini        BEGSR
062800010528     C*
062900010528     C* Inizializzo flag e contatori operativi
063000010528     C                   movel     '0'           flgGiro           1
063100010528     C                   movel     '0'           flgMulti          1
063200010528     C                   movel     '1'           flgStato          1
063300010615     C                   movel     '0'           flgOk             1
063400010528     C                   z-add     *zeros        cntNonEl         10 0
063500010528     C                   z-add     *zeros        depfil            3 0
063600010528     C                   z-add     *zeros        invfil            3 0
063700010528     C*
063800010528     C                   ENDSR
063900010528     C***
064000010528
064100010528
064200010528
064300010528
064400000613     C     *inzsr        BEGSR
064500990910     C*
064600990910     C     *entry        plist
064700990920     C                   parm                    tivlrds
064800990921     C                   parm      wrkesito      esito
064900000724     C                   parm                    prmlit
065000000710     C                   parm                    prmfir
065100011105     C*
065200011105     C* Campi riferito al database
065300011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
065400011105     C     *like         define    tbeke1        kteKE1
065500011105     C*
065600011105     C* Definizione chiavi di lettura
065700011105     C* TNTBE01L - parziale
065800011105     C     keytbe        klist
065900011105     C                   kfld                    kteCOD                         *UTENTE
066000011105     C                   kfld                    kteKE1                         *CODICE
066100011105     C*
066200011105     C* Reperimento data e ora corrente
066300110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
066400110502     C                   movel     wn14          oracor            6 0          *ORA (6)
066500110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066600110502     C                   eval      datcor = %dec(%date() : *ISO)
066700010529     C*
066800000613     C                   ENDSR
