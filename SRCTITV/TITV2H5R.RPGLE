000100110803      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente TEDDY - cod. 0240227)
000200990908     H dftactgrp(*yes)
000300990908
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
004300091026
004400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
004500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
004600091026
004700010528
004800010528
004900010528
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000724     C*
005400010528     C                   exsr      opeini
005500010528     C                   exsr      rwvao
005600110701     C*
005700110701     C* Esegue lancio TISI95R solo x chiusura
005800110701     C                   CLEAR                   TISI95DS
005900110701     C                   EVAL      I95TLA = 'C'
006000110701     C                   CALL      'TISI95R'
006100110701     C                   PARM                    TISI95DS
006200010528     C*
006300010528     C                   seton                                        lr
006400010528
006500010528
006600010528
006700010528
006800010528     C*--------------------------------------------------------
006900010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010528     C*--------------------------------------------------------
007100010528     C     PREELA        BEGSR
007200010528     C*
007300000724     C* SE OCCORRE SPEDIRE IN FILIALE
007400010528     C                   if        invfil <> *zeros and
007500010528     C                             flgGiro = '0'
007600010528     C*
007700010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010528     C                   eval      flgGiro = '1'
007900010529     C*
008000010529     C                   endif
008100010528     C*
008200010528     C                   ENDSR
008300010528     C***
008400010528
008500010528
008600010528     C*--------------------------------------------------------
008700010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008800010528     C*--------------------------------------------------------
008900010528     C     ENDELA        BEGSR
009000000613     C*
009100010528     C                   ENDSR
009200010528     C***
009300990908
009400010528
009500010528
009600910830     C*--------------------------------------------------------
009700020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
009800910830     C*--------------------------------------------------------
009900010122     C     RWVAO         BEGSR
010000010528     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400010122     C                   if        not %open(fnvaowwr)
010500010122     C                   open      fnvaowwr
010600990914     C                   endif
010700990910     C*
010800990910     C                   clear                   §CTROK
010900990910     C                   clear                   §CTRMO
011000990910     C                   clear                   §CTRNO
011100990910     C*
011200921023     C                   DO        *HIVAL
011300990913     C*
011400990915     C                   READ      tivin00r                               70
011500010731     C*
011600010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011700010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
011800010618     C*
011900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012000010618     C                   if        vinflg = '1'
012100010618     C                   eval      flgOk = '1'
012200010618     C                   endif
012300010618     C*
012400000905     C                   if        vindta > *blanks
012500000613     C                   add       1             rrnum
012600990913     C*
012700010711     C                   if        *in70 = *off and
012800010711     C                             (vinflg = *blanks
012900010711     C                              or vinflg = '0'
013000010711     C                              or vinflg = '2')
013100990913     C*
013200010528     C                   clear                   FNVAO000
013300011105     C                   clear                   TIORI000
013400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013500010711     C                   if        vinflg = *blanks or vinflg = '0'
013600010711     C                   clear                   vinmsg
013700010711     C                   endif
013800020507     C*
013900020507     C                   exsr      inzvar
014000020527     C                   exsr      defcam
014100020507     C                   exsr      impvao
014200010122     C*
014300010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400010528     C                   eval      depfil = VAOPOE
014500010528     C                   exsr      repfil
014600020507     C                   if        depfil = invfil
014700010528     C*
014800010528     C                   exsr      PREELA
014900010710     C*
015000930409     C*
015100010604     C  N31              ADD       1             §CTROK            7 0
015200010604     C   32              ADD       1             §CTRMO            7 0
015300010604     C   31              ADD       1             §CTRNO            7 0
015400020507     C*
015500030624     C  N31              EXSR      CHKCOR
015600011105     C  N31              EXSR      WRIORI
015700110503     C* N31              EXSR      WRIVGD
015800010122     C  N31              WRITE     FNVAO000
015900990910     C*
016000010604     C                   if        *in31 = *off and
016100010604     C                             *in32 = *off
016200990910     C                   eval      vinflg = '1'
016300990910     C                   else
016400990910     C                   eval      vinflg = '2'
016500991022     C                   endif
016600990910     C                   endif
016700010604     C*
016800010604     C                   endif
016900000905     C*
017000000905     C                   else
017100000905     C                   eval      vinflg = '1'
017200000905     C                   endif
017300000905     C*
017400000905     C  N70              update    tivin000
017500991022     C*
017600991022     C  N70              ENDdo
017700010528     C*
017800010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017900010528     C                   if        cntNonEl = *zeros or
018000010528     C                             flgMulti = '0'
018100010528     C* Se non ci sono record con errori ...
018200010528     C                   if        §ctrno = 0 and
018300010604     C                             §ctrmo = 0 and
018400010528     C                             flgStato <> '2'
018500990910     C* ... restituisco esito OK.
018600990921     C                   eval      wrkesito = '0'
018700990910     C                   else
018800000710     C                   if        §ctrok > 0
018900990921     C                   eval      wrkesito = '1'
019000000710     C                   else
019100010615     C                   if        flgOk = '0'
019200010615     C                   eval      wrkesito = '2'
019300010615     C                   else
019400010615     C                   eval      wrkesito = '6'
019500010615     C                   endif
019600990910     C                   endif
019700010528     C                   endif
019800010529     C                   else
019900010529     C                   eval      wrkesito = '9'
020000000710     C                   endif
020100990910     C*
020200990914     C                   if        %open(tivin00r)
020300990908     C                   close     tivin00r
020400990914     C                   endif
020500010122     C                   if        %open(fnvaowwr)
020600010122     C                   close     fnvaowwr
020700990914     C                   endif
020800990910     C*
020900010528     C                   if        vlrpoi <> 999
021000010528     C                   eval      invfil = vlrpoi
021100010528     C                   endif
021200010528     C*
021300990920     C                   if        §ctrok > 0
021400010528     C                             and invfil > *zeros
021500000613     C                   exsr      invio
021600990920     C                   endif
021700010612     C*
021800010612     C                   if        flgGiro = '1'
021900010612     C                   exsr      endela
022000010612     C                   endif
022100010528     C*
022200910830     C                   ENDSR
022300000613     C***
022400990920
022500020507
022600020507     C*----------------------------------------------------*
022700020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800020507     C*----------------------------------------------------*
022900020507     C     INZVAR        BEGSR
023000020507     C*
023100020507     C                   Z-ADD     *zeros        Num5_0            5 0
023200020507     C                   Z-ADD     *zeros        Num3_0            3 0
023300020507     C*
023400020507     C                   ENDSR
023500020507     C*----------------------------------------------------*
023600020507     C*  IMPOSTAZIONE CAMPI COSTANTI
023700020507     C*----------------------------------------------------*
023800020507     C     DEFCAM        BEGSR
023900020507     C*
024000020507     C* Imposto i valori di default...
024100110803     C                   EVAL      VAOPOE = 024
024200020507     C                   EVAL      VAOTOR = 'S'
024300020507     C                   EVAL      VAOTCO = 'F'
024400110803     C                   EVAL      VAOCOR = 0240227000
024500110502     C                   EVAL      VAOPAG = 'O'
024600110803     C                   EVAL      VAOKSC = 0240227
024700110503     C                   EVAL      VAODDT = 'S'
024800110803     C                   EVAL      VAOCTR = '100'
024900110803     C                   EVAL      VAOORR = 1400
025000110803     C                   EVAL      VAORER = 'GERENTE'
025100140225     C                   EVAL      ORIIDC = '00240227'
025200020507     C* ... e poi verifico se sono stati passati come parametri
025300020507     C                   IF        vlrppt > *blanks
025400020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
025500020507     C                   EXSR      CHKNUM
025600020507     C                   IF        PiInt=*on
025700020507     C                   Z-ADD     PiVal         VAOCOR
025800020507     C                   Z-ADD     PiVal         VAOCRC
025900110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
026000020507     C                   ENDIF
026100020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026200020507     C                   EXSR      CHKNUM
026300020507     C                   IF        PiInt=*on
026400020507     C                   Z-ADD     PiVal         VAOPOE
026500020507     C                   Z-ADD     PiVal         VAOPOC
026600020507     C                   ENDIF
026700020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026800020507     C                   EXSR      CHKNUM
026900020507     C                   IF        PiInt=*on
027000020507     C                   Z-ADD     PiVal         Num3_0
027100020507     C                   MOVEL     NUM3_0        VAOCTR
027200020507     C                   ENDIF
027300020507     C                   ENDIF
027400020507     C*
027500020507     C                   ENDSR
027600020507     C*----------------------------------------------------*
027700020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
027800020507     C*----------------------------------------------------*
027900020507     C     IMPVAO        BEGSR
028000020507     C*
028100020507     C                   SETOFF                                       3132
028200020507     C*
028300020507     C* Reperimento campi ALFA
028400020507     C*
028500110701     C* Dati x il ritiro
028600110803     C                   EVAL      VAORSR=%trim(%subst(vindta:219:40))
028700020507     C* == verifico se esiste il carattere @e, se c'è diventa A
028800020507     C     '@':'A'       XLATE     VAORSR        VAORSR
028900020507     C* ==
029000110803     C                   EVAL      VAOINR=%trim(%subst(vindta:259:35))
029100110803     C                   EVAL      VAOCAR=%trim(%subst(vindta:294:5))
029200110803     C                   EVAL      VAOLOR=%trim(%subst(vindta:299:40))
029300110803     C                   EVAL      VAOPRR=%trim(%subst(vindta:339:2))
029400110803     C                   EVAL      VAOTER=%trim(%subst(vindta:341:15))
029500110701     C* Reperisco la provincia dal CAP e dalla località
029600110701     C                   IF        VAOCAR <> *blanks AND
029700110701     C                             VAOPRR  = *blanks AND
029800110701     C                             VAONAR  = *blanks
029900110701     C                   CLEAR                   TISI95DS
030000110701     C                   EVAL      I95TCN = '3'
030100110701     C                   Z-ADD     datcor        I95DAT
030200110701     C                   EVAL      I95CAP = VAOCAR
030300110701     C                   EVAL      I95LOC = VAOLOR
030400110701     C                   CALL      'TISI95R'
030500110701     C                   PARM                    TISI95DS
030600110701     C                   EVAL      VAOPRR = O95PRV
030700110701     C                   ENDIF
030800110502     C*
030900110701     C* Dati x la consegna
031000110803     C                   EVAL      VAORSC=%trim(%subst(vindta:506:40))
031100110502     C* == verifico se esiste il carattere @e, se c'è diventa A
031200110502     C     '@':'A'       XLATE     VAORSC        VAORSC
031300110502     C* ==
031400110803     C                   EVAL      VAOINC=%trim(%subst(vindta:546:35))
031500110803     C                   EVAL      VAOCAC=%trim(%subst(vindta:581:5))
031600110803     C                   EVAL      VAOLOC=%trim(%subst(vindta:586:40))
031700110803     C                   EVAL      VAOPRC=%trim(%subst(vindta:626:2))
031800110502     C*
031900110701     C* Dati genericio ORM
032000110803     C                   EVAL      VAORFA=%trim(%subst(vindta:745:10))
032100110803     C                   EVAL      VAONO1=%trim(%subst(vindta:686:16))
032200110803     C*
032300140917     C***                SELECT
032400140917     C***                WHEN      %subst(vindta:709:1) = '1'
032500140917     C***                EVAL      VAOFFD='S'
032600140917     C***                ENDSL
032700020507     C*
032800020507     C* Reperimento campi NUMERICI
032900020507     C*
033000020527     C* Data e ora apertura chiamata
033100020527     C                   Z-ADD     datcor        VAODAO
033200071016     C                   MOVEL     wn14          VAOOAO
033300140922     C*
033400140922     C* Ordinante / pagante
033500140922     C                   EVAL      PiStr=%subst(vindta:1:7)
033600140922     C                   EXSR      CHKNUM
033700140922     C                   IF        PiInt=*on
033800140922     C                   Z-ADD     PiVal         VAOKSC
033900140922     C                   EVAL      VAOCOR = VAOKSC * 1000
034000140922     C                   ELSE
034100140922     C                   SETON                                        31
034200140922     C                   EVAL      VAOCOR = *zeros
034300140922     C                   EVAL      VAOKSC = *zeros
034400140922     C                   EVAL      vinmsg = %trimr(vinmsg)
034500140922     C                             + ' ' + 'VAOCOR VAOKSC'
034600140922     C                   ENDIF
034700020507     C*
034800020507     C* Data ritiro richiesta
034900110803     C                   EVAL      PiStr=%subst(vindta:682:4)+
035000110803     C                                   %subst(vindta:680:2)+
035100110803     C                                   %subst(vindta:678:2)
035200020507     C                   EXSR      CHKNUM
035300020507     C                   IF        PiInt=*on
035400020507     C                   Z-ADD     PiVal         VAODAR
035500020507     C                   ELSE
035600020507     C                   SETON                                        32
035700020507     C                   EVAL      VAODAR = *zeros
035800020507     C                   EVAL      vinmsg = %trimr(vinmsg)
035900020507     C                             + ' ' + 'VAODAR'
036000020507     C                   ENDIF
036100071012     C*
036200071012     C* Colli
036300110803     C                   EVAL      PiStr=%subst(vindta:723:5)
036400071012     C                   EXSR      CHKNUM
036500071012     C                   IF        PiInt=*on
036600071012     C                   Z-ADD     PiVal         VAONCL
036700071012     C                   ELSE
036800071012     C                   SETON                                        32
036900071012     C                   EVAL      VAONCL = *zeros
037000071012     C                   EVAL      vinmsg = %trimr(vinmsg)
037100071012     C                             + ' ' + 'VAONCL'
037200071012     C                   ENDIF
037300071012     C*
037400071012     C* Peso Kg
037500110803     C                   EVAL      PiStr=%subst(vindta:728:8)
037600071012     C                   EXSR      CHKNUM
037700071012     C                   IF        PiNum=*on
037800071012     C                   Z-ADD     PiVal         VAOPKG
037900071012     C                   ELSE
038000071012     C                   SETON                                        32
038100071012     C                   EVAL      VAOPKG = *zeros
038200071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038300071012     C                             + ' ' + 'VAOPKG'
038400071012     C                   ENDIF
038500110502     C*
038600110502     C* Volume M3
038700110803     C                   EVAL      PiStr=%subst(vindta:736:7)
038800110502     C                   EXSR      CHKNUM
038900110502     C                   IF        PiNum=*on
039000110502     C                   Z-ADD     PiVal         VAOVLM
039100110502     C                   ELSE
039200110502     C                   SETON                                        32
039300110502     C                   EVAL      VAOVLM = *zeros
039400110502     C                   EVAL      vinmsg = %trimr(vinmsg)
039500110502     C                             + ' ' + 'VAOVLM'
039600110502     C                   ENDIF
039700020507     C*
039800020507     C                   ENDSR
039900020507     C*----------------------------------------------------*
040000020507
040100020507
040200020507
040300020507     C*----------------------------------------------------*
040400020507     C*  CONTROLLO NUMERICITA' CAMPI
040500020507     C*----------------------------------------------------*
040600020507     C     CHKNUM        BEGSR
040700020507     C*
040800020507     C                   IF        PiDecChr = *blanks
040900020507     C                   EVAL      PiDecChr = '.'
041000020507     C                   ENDIF
041100020507     C*
041200020507     C                   CALL(e)   'ISNUMERIC'
041300020507     C                   PARM                    PiStr            30
041400020507     C                   PARM                    PiDecChr          1
041500020507     C                   PARM      *ZEROS        PiVal            30 9
041600020507     C                   PARM      '0'           PiInt             1
041700020507     C                   PARM      '0'           PiNum             1
041800020507     C                   IF        %error
041900020507     C                   EVAL      PiNum=*off
042000020507     C                   ENDIF
042100020507     C*
042200020507     C                   ENDSR
042300020507     C***
042400030624
042500030624
042600030624     C*------------------------------------------------------------------------*
042700030624     C* CHKCOR - Considerazioni su VAOCOR
042800030624     C*------------------------------------------------------------------------*
042900030624     C     CHKCOR        BEGSR
043000030624     C*
043100030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
043200030624     C                   if        vaoTCO = 'F'
043300030624     C* Quindi verifico se il codice ordinante non è valorizzato
043400030625     C                   if        vaoCOR = *zeros  AND
043500030625     C                             vaoRSO = *blanks AND
043600030625     C                             vaoINO = *blanks AND
043700030625     C                             vaoCAO = *blanks AND
043800030625     C                             vaoLOO = *blanks AND
043900030625     C                             vaoPRO = *blanks AND
044000030625     C                             vaoNAO = *blanks
044100030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
044200030624     C                   movel     '0'           wFlgAcr           1
044300030624     C                   move      vlrKSC        wCli              7 0
044400030624     C                   move(p)   wCli          acrCRO
044500030624     C     acrCRO        setll     fnacr01l
044600030624     C                   if        %found(fnacr01l)
044700030624     C                   read      fnacr01l
044800030624     C                   dow       not %eof(fnacr01l)
044900030624     C                   movel     acrCRO        wCliAcr           7 0
045000030624     C                   if        wCliAcr = wCli
045100030624     C                   eval      vaoCOR = acrCRO
045200030624     C                   movel     '1'           wFlgAcr
045300030624     C                   leave
045400030624     C                   else
045500030624     C                   leave
045600030624     C                   endif
045700030624     C                   read      fnacr01l
045800030624     C                   enddo
045900030624     C                   endif
046000030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
046100030624     C* dati anagrafici dell'ordinante dal piano dei conti
046200030624     C                   if        wFlgAcr = '0'
046300030624     C                   clear                   BS69DS
046400030624     C                   clear                   ACODS
046500030624     C                   clear                   INDDS
046600030624     C                   clear                   CLPDS
046700030624     C                   clear                   CLSDS
046800030624     C                   move(P)   vlrKSC        I69KAC
046900030624     C                   move(P)   vlrKSC        I69KIN
047000030624     C                   call      'TIBS69R'
047100030624     C                   parm                    BS69DS
047200030624     C                   parm                    ACODS
047300030624     C                   parm                    INDDS
047400030624     C                   parm                    CLPDS
047500030624     C                   parm                    CLSDS
047600030624     C     O69ERR        ifne      '1'
047700030624     C                   eval      vaoRSO = ACORAG
047800030624     C                   eval      vaoINO = INDVIA
047900030624     C                   movel(P)  INDCAP        vaoCAO
048000030624     C                   eval      vaoLOO = INDCIT
048100030624     C                   eval      vaoPRO = INDPRV
048200030624     C                   eval      vaoNAO = INDSTA
048300030624     C                   endif
048400030624     C                   endif
048500030624     C                   endif
048600030624     C                   endif
048700030624     C*
048800030624     C                   ENDSR
048900020507
049000020507
049100010528
049200011105     C*------------------------------------------------------------------------*
049300011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
049400011105     C*------------------------------------------------------------------------*
049500011105     C     WRIORI        BEGSR
049600011105     C*
049700090617     C* Come prima cosa stacco un numeratore da AZNUM
049800090617     C                   clear                   trul33ds
049900090617     C                   eval      I33OPE = *zeros
050000090617     C                   eval      I33CNU = 600
050100090617     C                   eval      I33NUM = 1
050200090617     C                   movel     TRUL33DS      KPJBU
050300090617     C                   call      'TRUL33R'
050400090617     C                   parm                    KPJBA
050500090617     C                   movel     KPJBU         TRUL33DS
050600090617     C                   if        O33ERR = *zeros
050700090617     C                   z-add     O33NRF        §numori
050800090618     c                   else
050900090618     c                   eval      §numori = *all'9'
051000090617     C                   endif
051100011105     C*
051200110502     C* Quindi imposto i campi "extra-VAO"
051300011105     C                   eval      oriPRG = §numori
051400011105     C                   eval      oriDER = datcor
051500110502     C                   eval      oriOER = oracor
051600011105     C                   eval      oriPOE = vaoPOE
051700011105     C                   eval      oriNSR = vaoNSR
051800011105     C                   eval      oriNOR = vaoNOR
051900011105     C                   eval      oriNRV = vaoNRV
052000011105     C                   eval      oriFLO = *blanks
052100011105     C*
052200011122     C* Infine valorizzo la chiave esterna sul file FNVAO
052300011105     C                   eval      dorm01 = vaoFLO
052400011105     C                   movel     §numori       §ormpg
052500110502     C                   movel     'S'           §ormfr
052600110803     C                   movel     'N'           §orcomc
052700011105     C                   eval      vaoFLO = dorm01
052800030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
052900030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
053000011105     C*
053100011105     C* ...quindi scrivo il file TIORI00F
053200011105     C                   WRITE     TIORI000
053300011105     C*
053400011105     C                   ENDSR
053500020507     C***
053600050826
053700050826
053800050826
053900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054000050826     C     wriVGD        BEGSR
054100050826     C*
054200050826     C* Reperisco la descrizione della fase da tabella
054300050826     C                   movel(p)  'FAR'         KteCOD
054400050826     C                   movel(p)  '000'         KteKE1
054500050826     C     KEYtbe        chain     tntbe01l
054600050826     C                   if        %found(tntbe01l)
054700050826     C                   movel(P)  tbeuni        DFAR
054800050826     C                   else
054900050826     C                   clear                   DFAR
055000050826     C                   endif
055100050826     C*
055200050826     C* Valorizzo buffer record
055300050826     C                   clear                   FNVAPDS
055400050826     C                   eval      vapIDC = oriIDC
055500050826     C                   eval      vapPOE = oriPOE
055600050826     C                   eval      vapRFA = vaoRFA
055700050826     C                   eval      vapPOG = oriPOE
055800050826     C                   eval      vapDAE = oriDER
055900050826     C                   eval      vapORE = oriOER
056000050826     C                   if        d§fardva <> *blanks
056100050826     C                   eval      vapDFA = d§fardva
056200050826     C                   else
056300050826     C                   eval      vapDFA = d§fardes
056400050826     C                   endif
056500050826     C*
056600050826     C                   clear                   tivgd000
056700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
056800050826     C                   eval      vgdTIP = 'VP'
056900050826     C                   eval      vgdKSU = vapIDC
057000050826     C                   eval      vgdTSC = 'WW'
057100050826     C                   eval      vgdDAT = datcor
057200110803     C                   eval      vgdPGM = 'TITV2H5R'
057300050826     C                   write     tivgd000
057400050826     C*
057500050826     C                   ENDSR
057600050826     C*------------------------------------------------------------------------*
057700011119
057800010528
057900010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058000010528     C     repfil        BEGSR
058100010528     C*
058200010528     C                   if        invfil = *zeros and
058300010528     C                             depfil > *zeros and
058400010613     C                             (vinflg = *blanks or
058500010613     C                              vinflg = *zeros)
058600010528     C
058700010528     C                   eval      invfil = depfil
058800010528     C                   endif
058900010528     C*
059000010529     C                   if        depfil <> invfil and
059100010529     C                             invfil > *zeros
059200010528     C                   eval      flgMulti = '1'
059300010528     C                   if        vinflg = *blanks
059400010528     C                   add       1             cntNonEl
059500010528     C                   endif
059600010528     C                   endif
059700010528     C*
059800010528     C                   if        vinflg = '2'
059900010528     C                   eval      flgStato = '2'
060000010528     C                   endif
060100010528     C*
060200010528     C                   ENDSR
060300010528     C***
060400010528
060500010528
060600010528
060700010528
060800990920      /TITLE Invio dei dati al punto operativo.
060900000613     C     invio         BEGSR
061000990920     C*
061100990920     C                   reset                   dscmz
061200010528     C                   move      invfil        cmzdst
061300990920     C                   eval      cmzfld = vlrfou
061400990920     C                   eval      cmzmbd = vlrhdl
061500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061600000710     C                   if        prmfir = *blanks
061700010122     C                   eval      cmzfla = 'FNVAO00F'
061800010122     C                   eval      cmzmba = 'FNVAO00F'
061900000710     C                   else
062000000710     C                   eval      cmzfla = prmfir
062100000710     C                   eval      cmzmba = prmfir
062200000710     C                   endif
062300990920     C                   eval      cmznrr = *zeros
062400990920     C                   move      §ctrok        cmznrr
062500010123     C                   eval      cmzlba = vlrfl1
062600990920     C                   call(e)   'TIS711C'
062700990920     C                   parm                    dscmz
062800990921     C                   parm      *blanks       esito
062900990920     C                   if        %error
063000990920     C                             or cmzerr = '1'
063100990921     C                             or esito  = '1'
063200000710     C                   eval      wrkesito = '3'
063300990920     C                   endif
063400990920     C*
063500000613     C                   ENDSR
063600000613     C***
063700990910
063800010528
063900010528
064000010528
064100010528      /TITLE Invio dei dati al punto operativo.
064200010528     C     opeini        BEGSR
064300010528     C*
064400010528     C* Inizializzo flag e contatori operativi
064500010528     C                   movel     '0'           flgGiro           1
064600010528     C                   movel     '0'           flgMulti          1
064700010528     C                   movel     '1'           flgStato          1
064800010615     C                   movel     '0'           flgOk             1
064900010528     C                   z-add     *zeros        cntNonEl         10 0
065000010528     C                   z-add     *zeros        depfil            3 0
065100010528     C                   z-add     *zeros        invfil            3 0
065200010528     C*
065300010528     C                   ENDSR
065400010528     C***
065500010528
065600010528
065700010528
065800010528
065900000613     C     *inzsr        BEGSR
066000990910     C*
066100990910     C     *entry        plist
066200990920     C                   parm                    tivlrds
066300990921     C                   parm      wrkesito      esito
066400000724     C                   parm                    prmlit
066500000710     C                   parm                    prmfir
066600011105     C*
066700011105     C* Campi riferito al database
066800011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
066900011105     C     *like         define    tbeke1        kteKE1
067000011105     C*
067100011105     C* Definizione chiavi di lettura
067200011105     C* TNTBE01L - parziale
067300011105     C     keytbe        klist
067400011105     C                   kfld                    kteCOD                         *UTENTE
067500011105     C                   kfld                    kteKE1                         *CODICE
067600011105     C*
067700011105     C* Reperimento data e ora corrente
067800110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
067900110502     C                   movel     wn14          oracor            6 0          *ORA (6)
068000110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068100110502     C                   eval      datcor = %dec(%date() : *ISO)
068200010529     C*
068300000613     C                   ENDSR
068400000613     C***
