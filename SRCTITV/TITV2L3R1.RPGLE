000100120605      /TITLE Upload via Internet: traduzione in FNVAOWWR.
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
003500110502     D §numori         s              7  0 inz
003600011122     D*-------------------
003700010731     D* COSTANTI
003800011122     D*-------------------
003900010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004000010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004100990908
004200091026
004300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
004400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
004500010528
004600010528
004700010528
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000000724     C*
005100010528     C                   exsr      opeini
005200010528     C                   exsr      rwvao
005300110701     C*
005400110701     C* Esegue lancio TISI95R solo x chiusura
005500110701     C                   CLEAR                   TISI95DS
005600110701     C                   EVAL      I95TLA = 'C'
005700110701     C                   CALL      'TISI95R'
005800110701     C                   PARM                    TISI95DS
005900010528     C*
006000010528     C                   seton                                        lr
006100010528
006200010528
006300010528     C*--------------------------------------------------------
006400010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010528     C*--------------------------------------------------------
006600010528     C     PREELA        BEGSR
006700010528     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900010528     C                   if        invfil <> *zeros and
007000010528     C                             flgGiro = '0'
007100010528     C*
007200010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010528     C                   eval      flgGiro = '1'
007400010529     C*
007500010529     C                   endif
007600010528     C*
007700010528     C                   ENDSR
007800010528     C***
007900010528
008000010528
008100010528     C*--------------------------------------------------------
008200010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008300010528     C*--------------------------------------------------------
008400010528     C     ENDELA        BEGSR
008500000613     C*
008600010528     C                   ENDSR
008700010528     C***
008800010528
008900010528
009000910830     C*--------------------------------------------------------
009100020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
009200910830     C*--------------------------------------------------------
009300010122     C     RWVAO         BEGSR
009400010528     C*
009500990914     C                   if        not %open(tivin00r)
009600990908     C                   open      tivin00r
009700990914     C                   endif
009800010122     C                   if        not %open(fnvaowwr)
009900010122     C                   open      fnvaowwr
010000990914     C                   endif
010100990910     C*
010200990910     C                   clear                   §CTROK
010300990910     C                   clear                   §CTRMO
010400990910     C                   clear                   §CTRNO
010500990910     C*
010600921023     C                   DO        *HIVAL
010700990913     C*
010800990915     C                   READ      tivin00r                               70
010900010731     C*
011000010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011100010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
011200010618     C*
011300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011400010618     C                   if        vinflg = '1'
011500010618     C                   eval      flgOk = '1'
011600010618     C                   endif
011700010618     C*
011800000905     C                   if        vindta > *blanks
011900990913     C*
012000010711     C                   if        *in70 = *off and
012100010711     C                             (vinflg = *blanks
012200010711     C                              or vinflg = '0'
012300010711     C                              or vinflg = '2')
012400990913     C*
012500010528     C                   clear                   FNVAO000
012600011105     C                   clear                   TIORI000
012700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012800010711     C                   if        vinflg = *blanks or vinflg = '0'
012900010711     C                   clear                   vinmsg
013000010711     C                   endif
013100020507     C*
013200020507     C                   exsr      inzvar
013300020527     C                   exsr      defcam
013400020507     C                   exsr      impvao
013500120807     C*
013600120807     C* Se record da considerare => procedo
013700120807     C                   if        *in33 = *on
013800120807     C                   eval      vinflg = '1'
013900120807     C                   else
014000010122     C*
014100010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014200010528     C                   eval      depfil = VAOPOE
014300010528     C                   exsr      repfil
014400020507     C                   if        depfil = invfil
014500010528     C*
014600010528     C                   exsr      PREELA
014700010710     C*
014800930409     C*
014900010604     C  N31              ADD       1             §CTROK            7 0
015000010604     C   32              ADD       1             §CTRMO            7 0
015100010604     C   31              ADD       1             §CTRNO            7 0
015200020507     C*
015300030624     C  N31              EXSR      CHKCOR
015400011105     C  N31              EXSR      WRIORI
015500110503     C* N31              EXSR      WRIVGD
015600010122     C  N31              WRITE     FNVAO000
015700990910     C*
015800010604     C                   if        *in31 = *off and
015900010604     C                             *in32 = *off
016000990910     C                   eval      vinflg = '1'
016100990910     C                   else
016200990910     C                   eval      vinflg = '2'
016300991022     C                   endif
016400990910     C                   endif
016500120807     C*
016600120807     C                   endif
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
023900120605     C*
024000120605     C                   SETOFF                                       55
024100020507     C*
024200020507     C* Imposto i valori di default...
024300120605     C                   EVAL      VAOPOE = 089
024400020507     C                   EVAL      VAOTOR = 'S'
024500020507     C                   EVAL      VAOTCO = 'F'
024600120606     C                   EVAL      VAOCOR = 0894539000
024700120606     C                   EVAL      VAOCRC = 0894539000
024800110502     C                   EVAL      VAOPAG = 'O'
024900120606     C                   EVAL      VAODDT = 'N'
025000120807     C                   EVAL      VAOKSC = 0894539
025100120807     C                   EVAL      VAOCTR = '000'
025200120606     C                   EVAL      ORIIDC = '00894539'
025300020507     C* ... e poi verifico se sono stati passati come parametri
025400020507     C                   IF        vlrppt > *blanks
025500020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
025600020507     C                   EXSR      CHKNUM
025700020507     C                   IF        PiInt=*on
025800020507     C                   Z-ADD     PiVal         VAOCOR
025900020507     C                   Z-ADD     PiVal         VAOCRC
026000110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
026100020507     C                   ENDIF
026200020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026300020507     C                   EXSR      CHKNUM
026400020507     C                   IF        PiInt=*on
026500020507     C                   Z-ADD     PiVal         VAOPOE
026600020507     C                   Z-ADD     PiVal         VAOPOC
026700020507     C                   ENDIF
026800020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026900020507     C                   EXSR      CHKNUM
027000020507     C                   IF        PiInt=*on
027100020507     C                   Z-ADD     PiVal         Num3_0
027200020507     C                   MOVEL     NUM3_0        VAOCTR
027300020507     C                   ENDIF
027400120605     C                   IF        %subst(vlrppt:17:1) = 'T'
027500120605     C                   SETON                                        55
027600120605     C                   EVAL      VAOPOE = 999
027700120605     C                   ENDIF
027800020507     C                   ENDIF
027900020507     C*
028000020507     C                   ENDSR
028100020507     C*----------------------------------------------------*
028200020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
028300020507     C*----------------------------------------------------*
028400020507     C     IMPVAO        BEGSR
028500020507     C*
028600120807     C                   SETOFF                                       313233
028700120605     C********
028800120605     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
028900120605     C********
029000120605     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
029100120807     C                   SETON                                        33
029200120605     C                   ENDIF
029300120605     C*
029400120605     C********
029500120605     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
029600120605     C********
029700120605     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
029800020507     C*
029900110701     C* Dati x il ritiro
030000120605     C                   EVAL      VAORSR=%trim(%subst(vindta:235:35))
030100020507     C* == verifico se esiste il carattere @e, se c'è diventa A
030200020507     C     '@':'A'       XLATE     VAORSR        VAORSR
030300020507     C* ==
030400120605     C                   EVAL      VAOINR=%trim(%subst(vindta:270:50))
030500120605     C                   EVAL      VAOLOR=%trim(%subst(vindta:320:35))
030600120605     C                   EVAL      VAOCAR=%trim(%subst(vindta:355:20))
030700120809     C                   EVAL      VAORER=%trim(%char(%dec(
030800120809     C                                    %trim(%subst(vindta:415:15)):15:0)))
030900120809     C                   EVAL      VAOTER=%trim(%char(%dec(
031000120809     C                                    %trim(%subst(vindta:430:15)):15:0)))
031100110701     C* Reperisco la provincia dal CAP e dalla località
031200110701     C                   IF        VAOCAR <> *blanks AND
031300110701     C                             VAOPRR  = *blanks AND
031400110701     C                             VAONAR  = *blanks
031500110701     C                   CLEAR                   TISI95DS
031600110701     C                   EVAL      I95TCN = '3'
031700110701     C                   Z-ADD     datcor        I95DAT
031800110701     C                   EVAL      I95CAP = VAOCAR
031900110701     C                   EVAL      I95LOC = VAOLOR
032000110701     C                   CALL      'TISI95R'
032100110701     C                   PARM                    TISI95DS
032200110701     C                   EVAL      VAOPRR = O95PRV
032300110701     C                   ENDIF
032400120605 xxx C*
032500120606     C* Dati x la consegna
032600120606     C***                EVAL      VAORSC=%trim(%subst(vindta:506:40))
032700110502     C* == verifico se esiste il carattere @e, se c'è diventa A
032800120606     C***  '@':'A'       XLATE     VAORSC        VAORSC
032900110502     C* ==
033000120606     C***                EVAL      VAOINC=%trim(%subst(vindta:546:35))
033100120606     C***                EVAL      VAOCAC=%trim(%subst(vindta:581:5))
033200120606     C***                EVAL      VAOLOC=%trim(%subst(vindta:586:40))
033300120606     C***                EVAL      VAOPRC=%trim(%subst(vindta:626:2))
033400110502     C*
033500110701     C* Dati genericio ORM
033600120605     C                   EVALR     VAORFA=%trim(%subst(vindta:61:20))
033700120605     C                   EVAL      VAONAM=%trim(%subst(vindta:81:20))
033800120605     C*
033900120605     C                   MOVEL     *blanks       wNOTE            70
034000120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
034100120605     C                                   %trim(%subst(vindta:955:300))
034200120605     C                   EVAL      VAONO1=%subst(wNOTE:1:35)
034300120605     C                   EVAL      VAONO2=%subst(wNOTE:36:34)
034400110803     C*
034500120606 xxx C***                SELECT
034600120606     C***                WHEN      %subst(vindta:709:1) = '1'
034700120606     C***                EVAL      VAOFFD='S'
034800120606     C***                ENDSL
034900020507     C*
035000020507     C* Reperimento campi NUMERICI
035100020507     C*
035200020527     C* Data e ora apertura chiamata
035300020527     C                   Z-ADD     datcor        VAODAO
035400071016     C                   MOVEL     wn14          VAOOAO
035500020507     C*
035600020507     C* Data ritiro richiesta
035700120605     C                   EVAL      PiStr=%subst(vindta:32:8)
035800020507     C                   EXSR      CHKNUM
035900020507     C                   IF        PiInt=*on
036000020507     C                   Z-ADD     PiVal         VAODAR
036100020507     C                   ELSE
036200020507     C                   SETON                                        32
036300020507     C                   EVAL      VAODAR = *zeros
036400020507     C                   EVAL      vinmsg = %trimr(vinmsg)
036500020507     C                             + ' ' + 'VAODAR'
036600020507     C                   ENDIF
036700120807     C*
036800120807     C* Ora ritiro richiesta
036900120807     C                   EVAL      PiStr=%subst(vindta:40:4)
037000120807     C                   EXSR      CHKNUM
037100120807     C                   IF        PiInt=*on
037200120807     C                   Z-ADD     PiVal         VAOORR
037300120807     C                   ELSE
037400120807     C                   SETON                                        32
037500120807     C                   EVAL      VAOORR = *zeros
037600120807     C                   EVAL      vinmsg = %trimr(vinmsg)
037700120807     C                             + ' ' + 'VAOORR'
037800120807     C                   ENDIF
037900071012     C*
038000071012     C* Colli
038100120605     C                   EVAL      PiStr=%subst(vindta:181:5)
038200071012     C                   EXSR      CHKNUM
038300071012     C                   IF        PiInt=*on
038400071012     C                   Z-ADD     PiVal         VAONCL
038500071012     C                   ELSE
038600071012     C                   SETON                                        32
038700071012     C                   EVAL      VAONCL = *zeros
038800071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038900071012     C                             + ' ' + 'VAONCL'
039000071012     C                   ENDIF
039100071012     C*
039200071012     C* Peso Kg
039300120605     C                   EVAL      PiStr=%subst(vindta:186:10)
039400071012     C                   EXSR      CHKNUM
039500071012     C                   IF        PiNum=*on
039600071012     C                   Z-ADD     PiVal         VAOPKG
039700071012     C                   ELSE
039800071012     C                   SETON                                        32
039900071012     C                   EVAL      VAOPKG = *zeros
040000071012     C                   EVAL      vinmsg = %trimr(vinmsg)
040100071012     C                             + ' ' + 'VAOPKG'
040200071012     C                   ENDIF
040300110502     C*
040400110502     C* Volume M3
040500120605     C                   EVAL      PiStr=%subst(vindta:196:14)
040600110502     C                   EXSR      CHKNUM
040700110502     C                   IF        PiNum=*on
040800110502     C                   Z-ADD     PiVal         VAOVLM
040900110502     C                   ELSE
041000110502     C                   SETON                                        32
041100110502     C                   EVAL      VAOVLM = *zeros
041200110502     C                   EVAL      vinmsg = %trimr(vinmsg)
041300110502     C                             + ' ' + 'VAOVLM'
041400110502     C                   ENDIF
041500120605     C*
041600120605     C                   ENDIF
041700120605     C*
041800120605     C********
041900120605     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
042000120605     C********
042100120605     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
042200120807     C                   SETON                                        33
042300120605     C                   ENDIF
042400020507     C*
042500020507     C                   ENDSR
042600020507     C*----------------------------------------------------*
042700020507
042800020507
042900020507
043000020507     C*----------------------------------------------------*
043100020507     C*  CONTROLLO NUMERICITA' CAMPI
043200020507     C*----------------------------------------------------*
043300020507     C     CHKNUM        BEGSR
043400020507     C*
043500020507     C                   IF        PiDecChr = *blanks
043600020507     C                   EVAL      PiDecChr = '.'
043700020507     C                   ENDIF
043800020507     C*
043900020507     C                   CALL(e)   'ISNUMERIC'
044000020507     C                   PARM                    PiStr            30
044100020507     C                   PARM                    PiDecChr          1
044200020507     C                   PARM      *ZEROS        PiVal            30 9
044300020507     C                   PARM      '0'           PiInt             1
044400020507     C                   PARM      '0'           PiNum             1
044500020507     C                   IF        %error
044600020507     C                   EVAL      PiNum=*off
044700020507     C                   ENDIF
044800020507     C*
044900020507     C                   ENDSR
045000020507     C***
045100030624
045200030624
045300030624     C*------------------------------------------------------------------------*
045400030624     C* CHKCOR - Considerazioni su VAOCOR
045500030624     C*------------------------------------------------------------------------*
045600030624     C     CHKCOR        BEGSR
045700030624     C*
045800030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
045900030624     C                   if        vaoTCO = 'F'
046000030624     C* Quindi verifico se il codice ordinante non è valorizzato
046100030625     C                   if        vaoCOR = *zeros  AND
046200030625     C                             vaoRSO = *blanks AND
046300030625     C                             vaoINO = *blanks AND
046400030625     C                             vaoCAO = *blanks AND
046500030625     C                             vaoLOO = *blanks AND
046600030625     C                             vaoPRO = *blanks AND
046700030625     C                             vaoNAO = *blanks
046800030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
046900030624     C                   movel     '0'           wFlgAcr           1
047000030624     C                   move      vlrKSC        wCli              7 0
047100030624     C                   move(p)   wCli          acrCRO
047200030624     C     acrCRO        setll     fnacr01l
047300030624     C                   if        %found(fnacr01l)
047400030624     C                   read      fnacr01l
047500030624     C                   dow       not %eof(fnacr01l)
047600030624     C                   movel     acrCRO        wCliAcr           7 0
047700030624     C                   if        wCliAcr = wCli
047800030624     C                   eval      vaoCOR = acrCRO
047900030624     C                   movel     '1'           wFlgAcr
048000030624     C                   leave
048100030624     C                   else
048200030624     C                   leave
048300030624     C                   endif
048400030624     C                   read      fnacr01l
048500030624     C                   enddo
048600030624     C                   endif
048700030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
048800030624     C* dati anagrafici dell'ordinante dal piano dei conti
048900030624     C                   if        wFlgAcr = '0'
049000030624     C                   clear                   BS69DS
049100030624     C                   clear                   ACODS
049200030624     C                   clear                   INDDS
049300030624     C                   clear                   CLPDS
049400030624     C                   clear                   CLSDS
049500030624     C                   move(P)   vlrKSC        I69KAC
049600030624     C                   move(P)   vlrKSC        I69KIN
049700030624     C                   call      'TIBS69R'
049800030624     C                   parm                    BS69DS
049900030624     C                   parm                    ACODS
050000030624     C                   parm                    INDDS
050100030624     C                   parm                    CLPDS
050200030624     C                   parm                    CLSDS
050300030624     C     O69ERR        ifne      '1'
050400030624     C                   eval      vaoRSO = ACORAG
050500030624     C                   eval      vaoINO = INDVIA
050600030624     C                   movel(P)  INDCAP        vaoCAO
050700030624     C                   eval      vaoLOO = INDCIT
050800030624     C                   eval      vaoPRO = INDPRV
050900030624     C                   eval      vaoNAO = INDSTA
051000030624     C                   endif
051100030624     C                   endif
051200030624     C                   endif
051300030624     C                   endif
051400030624     C*
051500030624     C                   ENDSR
051600020507
051700020507
051800010528
051900011105     C*------------------------------------------------------------------------*
052000011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
052100011105     C*------------------------------------------------------------------------*
052200011105     C     WRIORI        BEGSR
052300011105     C*
052400090617     C* Come prima cosa stacco un numeratore da AZNUM
052500090617     C                   clear                   trul33ds
052600090617     C                   eval      I33OPE = *zeros
052700090617     C                   eval      I33CNU = 600
052800090617     C                   eval      I33NUM = 1
052900090617     C                   movel     TRUL33DS      KPJBU
053000090617     C                   call      'TRUL33R'
053100090617     C                   parm                    KPJBA
053200090617     C                   movel     KPJBU         TRUL33DS
053300090617     C                   if        O33ERR = *zeros
053400090617     C                   z-add     O33NRF        §numori
053500090618     c                   else
053600090618     c                   eval      §numori = *all'9'
053700090617     C                   endif
053800011105     C*
053900110502     C* Quindi imposto i campi "extra-VAO"
054000011105     C                   eval      oriPRG = §numori
054100011105     C                   eval      oriDER = datcor
054200110502     C                   eval      oriOER = oracor
054300011105     C                   eval      oriPOE = vaoPOE
054400011105     C                   eval      oriNSR = vaoNSR
054500011105     C                   eval      oriNOR = vaoNOR
054600011105     C                   eval      oriNRV = vaoNRV
054700011105     C                   eval      oriFLO = *blanks
054800011105     C*
054900011122     C* Infine valorizzo la chiave esterna sul file FNVAO
055000011105     C                   eval      dorm01 = vaoFLO
055100011105     C                   movel     §numori       §ormpg
055200120807     C                   movel     'S'           §ormks
055300110502     C                   movel     'S'           §ormfr
055400120807     C                   movel     'S'           §ormfd
055500120605     C***                movel     'N'           §orcomc
055600011105     C                   eval      vaoFLO = dorm01
055700030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
055800030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
055900011105     C*
056000011105     C* ...quindi scrivo il file TIORI00F
056100120807     C  N55              WRITE     TIORI000
056200011105     C*
056300011105     C                   ENDSR
056400020507     C***
056500050826
056600050826
056700050826
056800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
056900050826     C     wriVGD        BEGSR
057000050826     C*
057100050826     C* Reperisco la descrizione della fase da tabella
057200050826     C                   movel(p)  'FAR'         KteCOD
057300050826     C                   movel(p)  '000'         KteKE1
057400050826     C     KEYtbe        chain     tntbe01l
057500050826     C                   if        %found(tntbe01l)
057600050826     C                   movel(P)  tbeuni        DFAR
057700050826     C                   else
057800050826     C                   clear                   DFAR
057900050826     C                   endif
058000050826     C*
058100050826     C* Valorizzo buffer record
058200050826     C                   clear                   FNVAPDS
058300050826     C                   eval      vapIDC = oriIDC
058400050826     C                   eval      vapPOE = oriPOE
058500050826     C                   eval      vapRFA = vaoRFA
058600050826     C                   eval      vapPOG = oriPOE
058700050826     C                   eval      vapDAE = oriDER
058800050826     C                   eval      vapORE = oriOER
058900050826     C                   if        d§fardva <> *blanks
059000050826     C                   eval      vapDFA = d§fardva
059100050826     C                   else
059200050826     C                   eval      vapDFA = d§fardes
059300050826     C                   endif
059400050826     C*
059500050826     C                   clear                   tivgd000
059600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
059700050826     C                   eval      vgdTIP = 'VP'
059800050826     C                   eval      vgdKSU = vapIDC
059900050826     C                   eval      vgdTSC = 'WW'
060000050826     C                   eval      vgdDAT = datcor
060100120605     C                   eval      vgdPGM = 'TITV2L3R1'
060200120605     C  N55              write     tivgd000
060300050826     C*
060400050826     C                   ENDSR
060500050826     C*------------------------------------------------------------------------*
060600011119
060700010528
060800010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
060900010528     C     repfil        BEGSR
061000010528     C*
061100010528     C                   if        invfil = *zeros and
061200010528     C                             depfil > *zeros and
061300010613     C                             (vinflg = *blanks or
061400010613     C                              vinflg = *zeros)
061500010528     C
061600010528     C                   eval      invfil = depfil
061700010528     C                   endif
061800010528     C*
061900010529     C                   if        depfil <> invfil and
062000010529     C                             invfil > *zeros
062100010528     C                   eval      flgMulti = '1'
062200010528     C                   if        vinflg = *blanks
062300010528     C                   add       1             cntNonEl
062400010528     C                   endif
062500010528     C                   endif
062600010528     C*
062700010528     C                   if        vinflg = '2'
062800010528     C                   eval      flgStato = '2'
062900010528     C                   endif
063000010528     C*
063100010528     C                   ENDSR
063200010528     C***
063300010528
063400010528
063500010528
063600010528
063700990920      /TITLE Invio dei dati al punto operativo.
063800000613     C     invio         BEGSR
063900990920     C*
064000990920     C                   reset                   dscmz
064100010528     C                   move      invfil        cmzdst
064200990920     C                   eval      cmzfld = vlrfou
064300990920     C                   eval      cmzmbd = vlrhdl
064400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
064500000710     C                   if        prmfir = *blanks
064600010122     C                   eval      cmzfla = 'FNVAO00F'
064700010122     C                   eval      cmzmba = 'FNVAO00F'
064800000710     C                   else
064900000710     C                   eval      cmzfla = prmfir
065000000710     C                   eval      cmzmba = prmfir
065100000710     C                   endif
065200990920     C                   eval      cmznrr = *zeros
065300990920     C                   move      §ctrok        cmznrr
065400010123     C                   eval      cmzlba = vlrfl1
065500990920     C                   call(e)   'TIS711C'
065600990920     C                   parm                    dscmz
065700990921     C                   parm      *blanks       esito
065800990920     C                   if        %error
065900990920     C                             or cmzerr = '1'
066000990921     C                             or esito  = '1'
066100000710     C                   eval      wrkesito = '3'
066200990920     C                   endif
066300990920     C*
066400000613     C                   ENDSR
066500000613     C***
066600990910
066700010528
066800010528
066900010528
067000010528      /TITLE Invio dei dati al punto operativo.
067100010528     C     opeini        BEGSR
067200010528     C*
067300010528     C* Inizializzo flag e contatori operativi
067400010528     C                   movel     '0'           flgGiro           1
067500010528     C                   movel     '0'           flgMulti          1
067600010528     C                   movel     '1'           flgStato          1
067700010615     C                   movel     '0'           flgOk             1
067800010528     C                   z-add     *zeros        cntNonEl         10 0
067900010528     C                   z-add     *zeros        depfil            3 0
068000010528     C                   z-add     *zeros        invfil            3 0
068100010528     C*
068200010528     C                   ENDSR
068300010528     C***
068400010528
068500010528
068600010528
068700010528
068800000613     C     *inzsr        BEGSR
068900990910     C*
069000990910     C     *entry        plist
069100990920     C                   parm                    tivlrds
069200990921     C                   parm      wrkesito      esito
069300000724     C                   parm                    prmlit
069400000710     C                   parm                    prmfir
069500011105     C*
069600011105     C* Campi riferito al database
069700011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
069800011105     C     *like         define    tbeke1        kteKE1
069900011105     C*
070000011105     C* Definizione chiavi di lettura
070100011105     C* TNTBE01L - parziale
070200011105     C     keytbe        klist
070300011105     C                   kfld                    kteCOD                         *UTENTE
070400011105     C                   kfld                    kteKE1                         *CODICE
070500011105     C*
070600011105     C* Reperimento data e ora corrente
070700110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
070800110502     C                   movel     wn14          oracor            6 0          *ORA (6)
070900110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071000110502     C                   eval      datcor = %dec(%date() : *ISO)
071100010529     C*
071200000613     C                   ENDSR
071300000613     C***
