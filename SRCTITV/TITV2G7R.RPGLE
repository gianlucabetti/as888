000100110701      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente DUCROS - cod. 0494494)
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
024100110701     C                   EVAL      VAOPOE = 049
024200020507     C                   EVAL      VAOTOR = 'S'
024300020507     C                   EVAL      VAOTCO = 'F'
024400110701     C                   EVAL      VAOCOR = 0494494000
024500110701     C                   EVAL      VAOCRC = 0494494001
024600110701     C                   EVAL      VAOFFD = 'S'
024700110502     C                   EVAL      VAOPAG = 'O'
024800110701     C                   EVAL      VAOKSC = 0494494
024900110503     C                   EVAL      VAODDT = 'S'
025000110701     C                   EVAL      VAOCTR = '001'
025100110701     C                   EVAL      VAOPOC = 049
025200110701     C                   EVAL      VAOORR = 1000
025300110701     C                   EVAL      ORIIDC = '00494494'
025400020507     C* ... e poi verifico se sono stati passati come parametri
025500020507     C                   IF        vlrppt > *blanks
025600020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
025700020507     C                   EXSR      CHKNUM
025800020507     C                   IF        PiInt=*on
025900020507     C                   Z-ADD     PiVal         VAOCOR
026000020507     C                   Z-ADD     PiVal         VAOCRC
026100110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
026200020507     C                   ENDIF
026300020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026400020507     C                   EXSR      CHKNUM
026500020507     C                   IF        PiInt=*on
026600020507     C                   Z-ADD     PiVal         VAOPOE
026700020507     C                   Z-ADD     PiVal         VAOPOC
026800020507     C                   ENDIF
026900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
027000020507     C                   EXSR      CHKNUM
027100020507     C                   IF        PiInt=*on
027200020507     C                   Z-ADD     PiVal         Num3_0
027300020507     C                   MOVEL     NUM3_0        VAOCTR
027400020507     C                   ENDIF
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
028800110701     C                   EVAL      VAORSR=%trim(%subst(vindta:256:35))
028900020507     C* == verifico se esiste il carattere @e, se c'è diventa A
029000020507     C     '@':'A'       XLATE     VAORSR        VAORSR
029100020507     C* ==
029200110701     C                   EVAL      VAOINR=%trim(%subst(vindta:291:35))
029300110701     C***                                 %trim(%subst(vindta:326:35))+
029400110701     C***                                 %trim(%subst(vindta:361:35))+
029500110701     C***                                 %trim(%subst(vindta:396:35))
029600110701     C                   EVAL      VAOCAR=%trim(%subst(vindta:398:9))
029700110701     C                   EVAL      VAOLOR=%trim(%subst(vindta:407:35))
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
031300110701     C***                EVAL      VAORSC=%trim(%subst(vindta:156:30))
031400110502     C* == verifico se esiste il carattere @e, se c'è diventa A
031500110502     C     '@':'A'       XLATE     VAORSC        VAORSC
031600110502     C* ==
031700110701     C***                EVAL      VAOINC=%trim(%subst(vindta:216:30))
031800110701     C***                EVAL      VAOCAC=%trim(%subst(vindta:398:9))
031900110701     C***                EVAL      VAOLOC=%trim(%subst(vindta:186:30))
032000110701     C***                EVAL      VAOPRC=%trim(%subst(vindta:254:5))
032100110502     C*
032200110701     C* Dati genericio ORM
032300110701     C                   EVAL      VAORER=%trim(%subst(vindta:657:17))
032400110701     C                   EVAL      VAOTER=%trim(%subst(vindta:674:15))
032500110701     C                   EVAL      VAORFA=%trim(%subst(vindta:1028:10))
032600110711     C                   EVAL      VAONAM=%trim(%subst(vindta:753:20))
032700110701     C                   MOVEL     *blanks       wNOTE            70
032800110706     C***                EVAL      wNOTE =%trim(%subst(vindta:863:55))+
032900110706     C***                                 %trim(%subst(vindta:918:55))+
033000110706     C***                                 %trim(%subst(vindta:973:55))
033100110706     C                   EVAL      wNOTE ='Ritiro per conto: ' +
033200110706     C                                 %trim(%subst(vindta:468:35))            +
033300110706     C***                            ' - '%trim(%subst(vindta:619:35))         +
033400110706     C                               ' ('+%trim(%subst(vindta:442:3)) +') Rif:'+
033500110706     C                                 %trim(%subst(vindta:1074:15))
033600110706     C                   EVAL      VAONO1=%trim(%subst(wNOTE:1:35))
033700110706     C                   EVAL      VAONO2=%trim(%subst(wNOTE:36:35))
033800020507     C*
033900020507     C* Reperimento campi NUMERICI
034000020507     C*
034100020527     C* Data e ora apertura chiamata
034200020527     C                   Z-ADD     datcor        VAODAO
034300071016     C                   MOVEL     wn14          VAOOAO
034400020507     C*
034500020507     C* Data ritiro richiesta
034600110701     C                   EVAL      PiStr=%subst(vindta:697:8)
034700020507     C                   EXSR      CHKNUM
034800020507     C                   IF        PiInt=*on
034900020507     C                   Z-ADD     PiVal         VAODAR
035000020507     C                   ELSE
035100020507     C                   SETON                                        32
035200020507     C                   EVAL      VAODAR = *zeros
035300020507     C                   EVAL      vinmsg = %trimr(vinmsg)
035400020507     C                             + ' ' + 'VAODAR'
035500020507     C                   ENDIF
035600110701     C*
035700110701     C* Ora ritiro richiesta
035800110701     C                   EVAL      PiStr=%subst(vindta:705:4)
035900110701     C                   EXSR      CHKNUM
036000110701     C                   IF        PiInt=*on
036100110701     C                   IF        PiVal > *zeros
036200110701     C                   Z-ADD     PiVal         VAOORR
036300110701     C                   ENDIF
036400110701     C                   ELSE
036500110701     C                   SETON                                        32
036600110701     C                   EVAL      VAOORR = *zeros
036700110701     C                   EVAL      vinmsg = %trimr(vinmsg)
036800110701     C                             + ' ' + 'VAOORR'
036900110701     C                   ENDIF
037000071012     C*
037100071012     C* Colli
037200110701     C                   EVAL      PiStr=%subst(vindta:729:5)
037300071012     C                   EXSR      CHKNUM
037400071012     C                   IF        PiInt=*on
037500071012     C                   Z-ADD     PiVal         VAONCL
037600071012     C                   ELSE
037700071012     C                   SETON                                        32
037800071012     C                   EVAL      VAONCL = *zeros
037900071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038000071012     C                             + ' ' + 'VAONCL'
038100071012     C                   ENDIF
038200071012     C*
038300071012     C* Peso Kg
038400110701     C                   EVAL      PiStr=%subst(vindta:734:9)
038500071012     C                   EXSR      CHKNUM
038600071012     C                   IF        PiNum=*on
038700110701     C***                EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
038800071012     C                   Z-ADD     PiVal         VAOPKG
038900110701     C                   IF        VAOPKG = *zeros
039000110701     C                   Z-ADD     0,1           VAOPKG
039100110701     C                   ENDIF
039200071012     C                   ELSE
039300071012     C                   SETON                                        32
039400071012     C                   EVAL      VAOPKG = *zeros
039500071012     C                   EVAL      vinmsg = %trimr(vinmsg)
039600071012     C                             + ' ' + 'VAOPKG'
039700071012     C                   ENDIF
039800110502     C*
039900110502     C* Volume M3
040000110701     C                   EVAL      PiStr=%subst(vindta:743:4)
040100110502     C                   EXSR      CHKNUM
040200110502     C                   IF        PiNum=*on
040300110701     C***                EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
040400110502     C                   Z-ADD     PiVal         VAOVLM
040500110502     C                   ELSE
040600110502     C                   SETON                                        32
040700110502     C                   EVAL      VAOVLM = *zeros
040800110502     C                   EVAL      vinmsg = %trimr(vinmsg)
040900110502     C                             + ' ' + 'VAOVLM'
041000110502     C                   ENDIF
041100020507     C*
041200020507     C                   ENDSR
041300020507     C*----------------------------------------------------*
041400020507
041500020507
041600020507
041700020507     C*----------------------------------------------------*
041800020507     C*  CONTROLLO NUMERICITA' CAMPI
041900020507     C*----------------------------------------------------*
042000020507     C     CHKNUM        BEGSR
042100020507     C*
042200020507     C                   IF        PiDecChr = *blanks
042300020507     C                   EVAL      PiDecChr = '.'
042400020507     C                   ENDIF
042500020507     C*
042600020507     C                   CALL(e)   'ISNUMERIC'
042700020507     C                   PARM                    PiStr            30
042800020507     C                   PARM                    PiDecChr          1
042900020507     C                   PARM      *ZEROS        PiVal            30 9
043000020507     C                   PARM      '0'           PiInt             1
043100020507     C                   PARM      '0'           PiNum             1
043200020507     C                   IF        %error
043300020507     C                   EVAL      PiNum=*off
043400020507     C                   ENDIF
043500020507     C*
043600020507     C                   ENDSR
043700020507     C***
043800030624
043900030624
044000030624     C*------------------------------------------------------------------------*
044100030624     C* CHKCOR - Considerazioni su VAOCOR
044200030624     C*------------------------------------------------------------------------*
044300030624     C     CHKCOR        BEGSR
044400030624     C*
044500030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
044600030624     C                   if        vaoTCO = 'F'
044700030624     C* Quindi verifico se il codice ordinante non è valorizzato
044800030625     C                   if        vaoCOR = *zeros  AND
044900030625     C                             vaoRSO = *blanks AND
045000030625     C                             vaoINO = *blanks AND
045100030625     C                             vaoCAO = *blanks AND
045200030625     C                             vaoLOO = *blanks AND
045300030625     C                             vaoPRO = *blanks AND
045400030625     C                             vaoNAO = *blanks
045500030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
045600030624     C                   movel     '0'           wFlgAcr           1
045700030624     C                   move      vlrKSC        wCli              7 0
045800030624     C                   move(p)   wCli          acrCRO
045900030624     C     acrCRO        setll     fnacr01l
046000030624     C                   if        %found(fnacr01l)
046100030624     C                   read      fnacr01l
046200030624     C                   dow       not %eof(fnacr01l)
046300030624     C                   movel     acrCRO        wCliAcr           7 0
046400030624     C                   if        wCliAcr = wCli
046500030624     C                   eval      vaoCOR = acrCRO
046600030624     C                   movel     '1'           wFlgAcr
046700030624     C                   leave
046800030624     C                   else
046900030624     C                   leave
047000030624     C                   endif
047100030624     C                   read      fnacr01l
047200030624     C                   enddo
047300030624     C                   endif
047400030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
047500030624     C* dati anagrafici dell'ordinante dal piano dei conti
047600030624     C                   if        wFlgAcr = '0'
047700030624     C                   clear                   BS69DS
047800030624     C                   clear                   ACODS
047900030624     C                   clear                   INDDS
048000030624     C                   clear                   CLPDS
048100030624     C                   clear                   CLSDS
048200030624     C                   move(P)   vlrKSC        I69KAC
048300030624     C                   move(P)   vlrKSC        I69KIN
048400030624     C                   call      'TIBS69R'
048500030624     C                   parm                    BS69DS
048600030624     C                   parm                    ACODS
048700030624     C                   parm                    INDDS
048800030624     C                   parm                    CLPDS
048900030624     C                   parm                    CLSDS
049000030624     C     O69ERR        ifne      '1'
049100030624     C                   eval      vaoRSO = ACORAG
049200030624     C                   eval      vaoINO = INDVIA
049300030624     C                   movel(P)  INDCAP        vaoCAO
049400030624     C                   eval      vaoLOO = INDCIT
049500030624     C                   eval      vaoPRO = INDPRV
049600030624     C                   eval      vaoNAO = INDSTA
049700030624     C                   endif
049800030624     C                   endif
049900030624     C                   endif
050000030624     C                   endif
050100030624     C*
050200030624     C                   ENDSR
050300020507
050400020507
050500010528
050600011105     C*------------------------------------------------------------------------*
050700011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
050800011105     C*------------------------------------------------------------------------*
050900011105     C     WRIORI        BEGSR
051000011105     C*
051100090617     C* Come prima cosa stacco un numeratore da AZNUM
051200090617     C                   clear                   trul33ds
051300090617     C                   eval      I33OPE = *zeros
051400090617     C                   eval      I33CNU = 600
051500090617     C                   eval      I33NUM = 1
051600090617     C                   movel     TRUL33DS      KPJBU
051700090617     C                   call      'TRUL33R'
051800090617     C                   parm                    KPJBA
051900090617     C                   movel     KPJBU         TRUL33DS
052000090617     C                   if        O33ERR = *zeros
052100090617     C                   z-add     O33NRF        §numori
052200090618     c                   else
052300090618     c                   eval      §numori = *all'9'
052400090617     C                   endif
052500011105     C*
052600110502     C* Quindi imposto i campi "extra-VAO"
052700011105     C                   eval      oriPRG = §numori
052800011105     C                   eval      oriDER = datcor
052900110502     C                   eval      oriOER = oracor
053000011105     C                   eval      oriPOE = vaoPOE
053100011105     C                   eval      oriNSR = vaoNSR
053200011105     C                   eval      oriNOR = vaoNOR
053300011105     C                   eval      oriNRV = vaoNRV
053400011105     C                   eval      oriFLO = *blanks
053500011105     C*
053600011122     C* Infine valorizzo la chiave esterna sul file FNVAO
053700011105     C                   eval      dorm01 = vaoFLO
053800011105     C                   movel     §numori       §ormpg
053900110502     C                   movel     'S'           §ormfr
054000110502     C                   movel     'S'           §ormks
054100110502     C                   movel     'S'           §ormfd
054200110701     C                   movel     'S'           §orcomc
054300011105     C                   eval      vaoFLO = dorm01
054400030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
054500030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
054600011105     C*
054700011105     C* ...quindi scrivo il file TIORI00F
054800011105     C                   WRITE     TIORI000
054900011105     C*
055000011105     C                   ENDSR
055100020507     C***
055200050826
055300050826
055400050826
055500050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
055600050826     C     wriVGD        BEGSR
055700050826     C*
055800050826     C* Reperisco la descrizione della fase da tabella
055900050826     C                   movel(p)  'FAR'         KteCOD
056000050826     C                   movel(p)  '000'         KteKE1
056100050826     C     KEYtbe        chain     tntbe01l
056200050826     C                   if        %found(tntbe01l)
056300050826     C                   movel(P)  tbeuni        DFAR
056400050826     C                   else
056500050826     C                   clear                   DFAR
056600050826     C                   endif
056700050826     C*
056800050826     C* Valorizzo buffer record
056900050826     C                   clear                   FNVAPDS
057000050826     C                   eval      vapIDC = oriIDC
057100050826     C                   eval      vapPOE = oriPOE
057200050826     C                   eval      vapRFA = vaoRFA
057300050826     C                   eval      vapPOG = oriPOE
057400050826     C                   eval      vapDAE = oriDER
057500050826     C                   eval      vapORE = oriOER
057600050826     C                   if        d§fardva <> *blanks
057700050826     C                   eval      vapDFA = d§fardva
057800050826     C                   else
057900050826     C                   eval      vapDFA = d§fardes
058000050826     C                   endif
058100050826     C*
058200050826     C                   clear                   tivgd000
058300080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
058400050826     C                   eval      vgdTIP = 'VP'
058500050826     C                   eval      vgdKSU = vapIDC
058600050826     C                   eval      vgdTSC = 'WW'
058700050826     C                   eval      vgdDAT = datcor
058800110701     C                   eval      vgdPGM = 'TITV2G7R'
058900050826     C                   write     tivgd000
059000050826     C*
059100050826     C                   ENDSR
059200050826     C*------------------------------------------------------------------------*
059300011119
059400010528
059500010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059600010528     C     repfil        BEGSR
059700010528     C*
059800010528     C                   if        invfil = *zeros and
059900010528     C                             depfil > *zeros and
060000010613     C                             (vinflg = *blanks or
060100010613     C                              vinflg = *zeros)
060200010528     C
060300010528     C                   eval      invfil = depfil
060400010528     C                   endif
060500010528     C*
060600010529     C                   if        depfil <> invfil and
060700010529     C                             invfil > *zeros
060800010528     C                   eval      flgMulti = '1'
060900010528     C                   if        vinflg = *blanks
061000010528     C                   add       1             cntNonEl
061100010528     C                   endif
061200010528     C                   endif
061300010528     C*
061400010528     C                   if        vinflg = '2'
061500010528     C                   eval      flgStato = '2'
061600010528     C                   endif
061700010528     C*
061800010528     C                   ENDSR
061900010528     C***
062000010528
062100010528
062200010528
062300010528
062400990920      /TITLE Invio dei dati al punto operativo.
062500000613     C     invio         BEGSR
062600990920     C*
062700990920     C                   reset                   dscmz
062800010528     C                   move      invfil        cmzdst
062900990920     C                   eval      cmzfld = vlrfou
063000990920     C                   eval      cmzmbd = vlrhdl
063100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063200000710     C                   if        prmfir = *blanks
063300010122     C                   eval      cmzfla = 'FNVAO00F'
063400010122     C                   eval      cmzmba = 'FNVAO00F'
063500000710     C                   else
063600000710     C                   eval      cmzfla = prmfir
063700000710     C                   eval      cmzmba = prmfir
063800000710     C                   endif
063900990920     C                   eval      cmznrr = *zeros
064000990920     C                   move      §ctrok        cmznrr
064100010123     C                   eval      cmzlba = vlrfl1
064200990920     C                   call(e)   'TIS711C'
064300990920     C                   parm                    dscmz
064400990921     C                   parm      *blanks       esito
064500990920     C                   if        %error
064600990920     C                             or cmzerr = '1'
064700990921     C                             or esito  = '1'
064800000710     C                   eval      wrkesito = '3'
064900990920     C                   endif
065000990920     C*
065100000613     C                   ENDSR
065200000613     C***
065300990910
065400010528
065500010528
065600010528
065700010528      /TITLE Invio dei dati al punto operativo.
065800010528     C     opeini        BEGSR
065900010528     C*
066000010528     C* Inizializzo flag e contatori operativi
066100010528     C                   movel     '0'           flgGiro           1
066200010528     C                   movel     '0'           flgMulti          1
066300010528     C                   movel     '1'           flgStato          1
066400010615     C                   movel     '0'           flgOk             1
066500010528     C                   z-add     *zeros        cntNonEl         10 0
066600010528     C                   z-add     *zeros        depfil            3 0
066700010528     C                   z-add     *zeros        invfil            3 0
066800010528     C*
066900010528     C                   ENDSR
067000010528     C***
067100010528
067200010528
067300010528
067400010528
067500000613     C     *inzsr        BEGSR
067600990910     C*
067700990910     C     *entry        plist
067800990920     C                   parm                    tivlrds
067900990921     C                   parm      wrkesito      esito
068000000724     C                   parm                    prmlit
068100000710     C                   parm                    prmfir
068200011105     C*
068300011105     C* Campi riferito al database
068400011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
068500011105     C     *like         define    tbeke1        kteKE1
068600011105     C*
068700011105     C* Definizione chiavi di lettura
068800011105     C* TNTBE01L - parziale
068900011105     C     keytbe        klist
069000011105     C                   kfld                    kteCOD                         *UTENTE
069100011105     C                   kfld                    kteKE1                         *CODICE
069200011105     C*
069300011105     C* Reperimento data e ora corrente
069400110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
069500110502     C                   movel     wn14          oracor            6 0          *ORA (6)
069600110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069700110502     C                   eval      datcor = %dec(%date() : *ISO)
069800010529     C*
069900000613     C                   ENDSR
070000000613     C***
