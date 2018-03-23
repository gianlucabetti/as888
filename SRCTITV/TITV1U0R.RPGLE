000100091223      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400091223     Ftabel00f  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700091223     FEDIVABwr  O    E             DISK    usropn
000800091223     FEDIVATwr  O    E             DISK    usropn
000900091223     Ftitv1u0p  O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200091223     Ftitv1u0ps O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070411     F                                     infsr(*pssr)
001500990908
001600000512     D*------------
001700000512     D* COMANDI
001800000512     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000000801     D*----------------------------------------------------
002100000801     D* DICHIARAZIOINE VARIABILI DI WRK
002200000801     D*----------------------------------------------------
002300990920     D dscmz         e ds                  inz
002400990910     D psds           sds
002500990910     D  procname         *PROC
002600990920     D tivlrds       e ds                  extname(tivlr00f)
002700091223     D ds15          e ds
002800990910     D esito           s              1
002900000724     D prmlit          s             10
003000000710     D prmfir          s             10
003100990921     D wrkesito        s                   like(esito)
003200990915     D wrkdata         s               d
003300990915     D wrkora          s               t
003400000613     D rrnum           s              6  0 INZ(*zeros)
003500000621     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700010202     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100091214     D curSped         s             35    INZ(*blanks)
004200091214     D depSped         s             35    INZ(*blanks)
004300091223     D jNAZ            s              5  0 INZ(*zeros)
004400091223     D wNomeFile       s             30    INZ(*blanks)
004500091223
004600091223     D*------------
004700091223     D* SCHIERE
004800091223     D*------------
004900091223     D skNAZISO        S              3    DIM(1000)
005000091223     D skNAZBAR        S              3    DIM(1000)
005100090114
005200091215     D*------------------
005300091215     D* DS REPERIMENTO NUMERATORE
005400091215     D*------------------
005500091215     D trul33ds      e ds                  inz
005600091215     D*------------------
005700091215     D* DS ARCHITETTURA
005800091215     D*------------------
005900091215     D kpjba         e ds                  inz
006000091215
006100081217
006200081217     D*------------------
006300081217     D* LINKING A DEFINIZIONI ESTERNE
006400081217     D*------------------
006500081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700081217
006800081217
006900010201
007000081217
007100091223     C*
007200091223     C* Carico le tabelle
007300091223     C                   exsr      cartab
007400091223     C*
007500081217     C
007600990915     C                   time                    wrkdata
007700990915     C                   time                    wrkora
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000724     C*
008200000724     C* SE OCCORRE SPEDIRE IN FILIALE
008300000724     C                   if        vlrpoi <> *zeros
008400000724     C*
008500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000724     C     vlrpoi        chain     azorg01l
008800000724     C                   if        %found
008900000616     C                   movel(p)  CMD(1)        depcmd
009000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000724     C*
009800000724     C                   endif
009900000724     C                   endif
010000000616     C*
010100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200000616     C                   movel(p)  CMD(2)        depcmd
010300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010600011113     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000616     C*
011100091223     C                   if        not %open(titv1u0ps)
011200091223     C                   open      titv1u0ps
011300000616     C                   except    testdett
011400000613     C                   endif
011500000613     C*
011600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900010202     C* Effettuo la chiamata al CLLE preposto
012000091223     C                   call(e)   'TITVEVTC'
012100010202     C                   parm                    parccm
012200010202     C                   parm                    parmbr
012300010202     C                   parm      '2'           paropz
012400010202     C*
012500091223     C                   if        %open(titv1u0ps)
012600000616     C                   except    findett
012700091223     C                   close     titv1u0ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600000801     C
013700010201     C                   seton                                        LR
013800000613
013900000613
014000000613     C*--------------------------------------------------------
014100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014200000613     C*--------------------------------------------------------
014300000621     C     STPR          BEGSR
014400000613     C*
014500091223     C                   if        not %open(titv1u0p)
014600091223     C                   open      titv1u0p
014700990915     C                   endif
014800990915     C*
014900990915     C                   except    riepilogo
015000990915     C*
015100091223     C                   if        %open(titv1u0p)
015200091223     C                   close     titv1u0p
015300990914     C                   endif
015400990910     C*
015500000613     C                   ENDSR
015600000613     C***
015700990908
015800000801
015900910830     C*--------------------------------------------------------
016000091223     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
016100910830     C*--------------------------------------------------------
016200040526     C     RWFILE        BEGSR
016300990910     C*
016400990914     C                   if        not %open(tivin00r)
016500990908     C                   open      tivin00r
016600990914     C                   endif
016700091223     C                   if        not %open(edivabwr)
016800091223     C                   open      edivabwr
016900990914     C                   endif
017000070103     C*
017100091223     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
017200020305     C                   exsr      prevat
017300010201     C*
017400010202     C                   if        chkcall = '0'
017500010202     C*
017600091223     C                   if        not %open(edivatwr)
017700091223     C                   open      edivatwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   clear                   §CTROKVB          5 0
018100020305     C                   clear                   §CTROKVT          5 0
018200000801     C                   clear                   §CTRMO            5 0
018300000801     C                   clear                   §CTRNO            5 0
018400990910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800050627     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000801     C*
019100091214     C                   if        *in70 = *off
019200000801     C                             and
019300000801     C                             (vinflg = *blanks
019400000801     C                              or vinflg = '0'
019500000801     C                              or vinflg = '2')
019600000801     C*
019700000801     C                   clear                   vinmsg
019800000801     C                   eval      vinflg = '1'
019900070103     C*
020000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
020100091223     C                   eval      curSped = %subst(vindta:1:12)
020200070213     C                   if        curSped <> depSped
020300070213     C* Se prima bolla => importo bolla corrente
020400070213     C                   if        §CTRMO = *zeros
020500091215     C                   exsr      repNSP
020600070213     C                   exsr      impvab                                       => carico  VAB
020700070213     C                   else
020800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020900070213     C                   exsr      wrivab                                       => scarico VAB
021000091215     C                   exsr      repNSP
021100070213     C                   exsr      impvab                                       => carico  VAB
021200070213     C                   endif
021300070103     C* Salvo il raggruppamento spedizione cliente corrente
021400070103     C                   eval      depSped = curSped
021500070213     C*
021600070213     C* Se collo successivo x stessa bolla
021700070213     C                   else
021800070213     C                   exsr      impvab                                       => carico  VAB
021900070103     C                   endif
022000091215     C*
022100091215     C                   exsr      exevate                                      => write VAT-E
022200000905     C*
022300000905     C                   else
022400000905     C                   eval      vinflg = '1'
022500050628     C                   endif
022600000905     C                   endif
022700000905     C*
022800000905     C  N70              update    tivin000
022900000905     C*
023000991022     C  N70              ENDdo
023100070213     C*
023200070213     C* Scarico testata bolla rimasta in sospesa
023300070213     C                   exsr      wrivab                                       => scarico VAB
023400010202     C*
023500010202     C                   endif
023600990910
023700990910     C* Se non ci sono record con errori ...
023800000710     C                   if        §ctrno = 0
023900990910     C* ... restituisco esito OK.
024000990921     C                   eval      wrkesito = '0'
024100990910     C                   else
024200010201     C                   if        §ctrokvb > 0
024300990921     C                   eval      wrkesito = '1'
024400000710     C                   else
024500000710     C                   eval      wrkesito = '2'
024600990910     C                   endif
024700000710     C                   endif
024800990910     C*
024900990914     C                   if        %open(tivin00r)
025000990908     C                   close     tivin00r
025100990914     C                   endif
025200091223     C                   if        %open(edivabwr)
025300091223     C                   close     edivabwr
025400990914     C                   endif
025500091223     C                   if        %open(edivatwr)
025600091223     C                   close     edivatwr
025700010201     C                   endif
025800990910     C*
025900010201     C                   if        §ctrokvb > 0
026000000724     C                             and vlrpoi <> *zeros
026100010202     C                   exsr      invio
026200990920     C                   endif
026300990920     C*
026400910830     C                   ENDSR
026500000613     C***
026600010305
026700010305     C*----------------------------------------------------*
026800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026900010305     C*----------------------------------------------------*
027000020305     C     WRIVAB        BEGSR
027100091223     C*
027200091223     C* VALORIZZO CAMPI RELATIVI AL "CMR"
027300091223     C                   eval      VABCMR = wNomeFile
027400091223     C                   eval      VABDCM = DATCOR
027500091223     C                   eval      VABDTS = DATCOR
027600091223     C                   eval      VABHMS = ORACOR
027700091223     C                   eval      VABCNT = 1
027800010305     C*
027900060225     C* Quindi scarico il buffer del file d testata
028000091223     C                   write     edivab00                                     => scarico il VAB
028100010305     C*
028200010305     C                   ENDSR
028300990920
028400000801     C*----------------------------------------------------*
028500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028600000801     C*----------------------------------------------------*
028700010201     C     INZVAR        BEGSR
028800000801     C*
028900091223     C                   CLEAR                   EDIVAB00
029000091223     C                   CLEAR                   EDIVAT00
029100070213     C*
029200040802     C                   Z-ADD     *zeros        Num5_0            5 0
029300040802     C                   MOVEL     '0'           FlgCAS            1
029400000801     C*
029500000801     C                   ENDSR
029600000801     C*----------------------------------------------------*
029700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029800000801     C*----------------------------------------------------*
029900000801     C     DEFCAM        BEGSR
030000000801     C*
030100020619     C* Imposto i valori di default...
030200091223     C                   Z-ADD     0633315       VABCCM
030300091223     C                   Z-ADD     0633315       VATCCM
030400091223     C                   Z-ADD     063           VABLNP
030500091223     C                   Z-ADD     063           VATLNP
030600091223     C                   Z-ADD     000           VABCTR
030700091223     C                   Z-ADD     1             VABNCL
030800040823     C                   MOVEL     '1'           VABCBO
030900020619     C* ... e poi verifico se sono stati passati come parametri
031000020619     C                   IF        vlrppt > *blanks
031100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
031200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031300020619     C                   EXSR      CHKNUM
031400020619     C                   IF        PiInt=*on
031500020619     C                   Z-ADD     PiVal         VABCCM
031600020619     C                   Z-ADD     PiVal         VATCCM
031700020619     C                   ENDIF
031800040506     C                   ENDIF
031900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
032000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020619     C                   EXSR      CHKNUM
032200020619     C                   IF        PiInt=*on
032300020619     C                   Z-ADD     PiVal         VABLNP
032400020619     C                   Z-ADD     PiVal         VATLNP
032500040506     C                   ENDIF
032600020619     C                   ENDIF
032700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
032800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032900020619     C                   EXSR      CHKNUM
033000020619     C                   IF        PiInt=*on
033100020619     C                   Z-ADD     PiVal         VABCTR
033200040506     C                   ENDIF
033300020619     C                   ENDIF
033400020619     C                   ENDIF
033500000801     C*
033600000801     C                   ENDSR
033700091215     C*----------------------------------------------------*
033800091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033900091215     C*----------------------------------------------------*
034000091215     C     REPNSP        BEGSR
034100091215     C*
034200091215     C                   EXSR      INZVAR
034300091215     C                   EXSR      DEFCAM
034400091215     C*
034500091215     C* NSP => Stacco un numeratore da AZNUM
034600091215     C                   clear                   TRUL33DS
034700091215     C                   eval      I33OPE = *zeros
034800091215     C                   eval      I33CNU = 302
034900091215     C                   eval      I33NUM = 1
035000091215     C                   movel     TRUL33DS      KPJBU
035100091215     C                   call      'TRUL33R'
035200091215     C                   parm                    KPJBA
035300091215     C                   movel     KPJBU         TRUL33DS
035400091215     C                   if        O33ERR = *zeros
035500091215     C                   z-add     O33NRF        VABNSP
035600091215     C                   z-add     O33NRF        VATNSP
035700091215     C                   else
035800091215     C                   ADD       1             errore
035900091215     C                   EVAL      vinmsg = %trimr(vinmsg)
036000091215     C                             + ' ' + 'VABNSP VATNSP'
036100091215     C                   endif
036200091215     C*
036300091215     C                   ENDSR
036400000801     C*----------------------------------------------------*
036500091223     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
036600000801     C*----------------------------------------------------*
036700040823     C     IMPVAB        BEGSR
036800010305     C*
036900000801     C                   Z-ADD     *zeros        errore            1 0
037000000830     C                   MOVEL     datcor        VABAAS
037100020305     C                   MOVEL     datcor        VATAAS
037200040526     C                   MOVE      datcor        VABMGS
037300040823     C                   MOVE(P)   vlrpoi        VABFGS
037400040823     C                   MOVE(P)   vlrpoi        VATFGS
037500080409     C*
037600080409     C* Reperimento campi ALFA
037700080409     C*
037800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
037900091223     C                   MOVEL     *blanks       wRSD             70
038000091223     C                   EVAL      wRSD=%trim(%subst(vindta:58:45)) +' '+
038100091223     C                                  %trim(%subst(vindta:103:45))+' '+
038200091223     C                                  %trim(%subst(vindta:148:45))
038300091223     C                   EVAL      VABRSD=%subst(wRSD:1:35)
038400080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038500080409     C     '@':'A'       XLATE     VABRSD        VABRSD
038600080409     C* ==
038700091223     C                   EVAL      VABRD2=%subst(wRSD:36:35)
038800091223     C                   EVAL      VABIND=%trim(%subst(vindta:193:45))
038900091223     C                   MOVEL     *blanks       wCAPLOCPRV       45
039000091223     C                   MOVEL     *blanks       wNAZ             45
039100091223     C                   MOVEL     *blanks       wNZD              3
039200091223     C                   EVAL      wCAPLOCPRV=%trim(%subst(vindta:238:45))
039300091223     C                   EVAL      wNAZ=%trim(%subst(vindta:283:45))
039400091223     C*
039500091223     C* Se 'IT' ....  ovviamente è Italia
039600091223     C                   IF        %len(%trim(wNAZ)) >= 3 and
039700091223     C                             %trim(%subst(wNAZ:%len(%trim(wNAZ))-3+1:3))=
039800091223     C                             'IT'
039900091223     C                   EVAL      wNAZ=*blanks
040000091223     C                   ENDIF
040100091223     C*
040200091223     C* ...trafffici Italia
040300091223     C                   Z-ADD     *zeros        wPos              3 0
040400091223     C                   IF        wNAZ = *blanks
040500091223     C                   EVAL      VABPRD=%trim(%subst(wCAPLOCPRV:
040600091223     C                                    %len(%trim(wCAPLOCPRV))-2:3))
040700091223     C* CAD
040800091223     C                   EVAL      PiStr=%trim(%subst(wCAPLOCPRV:1:6))
040900091223     C                   EXSR      CHKNUM
041000091223     C                   IF        PiInt=*on
041100091223     C                   Z-ADD     PiVal         Num5_0
041200091223     C                   MOVEL     Num5_0        VABCAD
041300091223     C                   EVAL      VABLOD=%trim(%subst(wCAPLOCPRV:6:
041400091223     C                                    %len(%trim(wCAPLOCPRV))-3-6+1))
041500091223     C                   ELSE
041600091223     C                   EVAL      VABLOD=%trim(%subst(wCAPLOCPRV:1:
041700091223     C                                    %len(%trim(wCAPLOCPRV))-3+1))
041800091223     C                   ENDIF
041900091223     C*
042000091223     C* ...trafffici estero
042100091223     C                   ELSE
042200091223     C                   EVAL      wPos=%scan(' ':wCAPLOCPRV)
042300091223     C                   EVAL      VABCAD=%trim(%subst(wCAPLOCPRV:1:wPos))
042400091223     C                   EVAL      VABLOD=%trim(%subst(wCAPLOCPRV:wPos))
042500091223     C                   EVAL      wNZD=%trim(%subst(wNAZ:
042600091223     C                                  %len(%trim(wNAZ))-2:3))
042700091223     C* ...passo dal codice ISO a quello "Bartolini"
042800091223     C                   Z-ADD     1             jNAZ
042900091223     C     wNZD          LOOKUP    skNAZISO(jNAZ)                         55
043000091223     C                   IF        %found
043100091223     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
043200091223     C                   ENDIF
043300091223     C                   ENDIF
043400091223     C*
043500091223     C                   EVAL      VABRMA=%trim(%subst(vindta:1:12))
043600080409     C*
043700080409     C* Reperimento campi NUMERICI
043800090309     C* RMN
043900091223     C                   EVAL      PiStr=%trim(%subst(vindta:2:2))+
044000091223     C                                   %trim(%subst(vindta:7:6))
044100091215     C                   EXSR      CHKNUM
044200091215     C                   IF        PiInt=*on
044300091215     C                   Z-ADD     PiVal         VABRMN
044400091215     C                   ELSE
044500091215     C                   ADD       1             errore
044600091215     C                   Z-ADD     1             VABRMN
044700091215     C                   EVAL      vinmsg = %trimr(vinmsg)
044800091215     C                             + ' ' + 'VABRMN'
044900091215     C                   ENDIF
045000091223     C* PKB
045100091223     C                   EVAL      PiStr=%trim(%subst(vindta:329:5))
045200091223     C                   EXSR      CHKNUM
045300091223     C                   IF        PiNum=*on
045400091223     C                   Z-ADD(H)  PiVal         VABPKB
045500091223     C                   ELSE
045600091223     C                   SETON                                        32
045700091223     C                   EVAL      vinmsg = %trimr(vinmsg)
045800091223     C                             + ' ' + 'VABPKB'
045900091223     C                   ENDIF
046000071121     C*
046100080512     C                   exsr      exevata
046200080512     C                   exsr      exevatb
046300010205     C*
046400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046500040802     C                   IF        FlgCAS <> '0'
046600090922     C                   IF        VABCBO = '1'
046700010205     C                   EVAL      VABCBO = '4'
046800090922     C                   ELSE
046900090922     C                   EVAL      VABCBO = '6'
047000090922     C                   ENDIF
047100010205     C                   ENDIF
047200020305     C*
047300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047400011113     C                   EXSR      CHKIMPDIV
047500010202     C*
047600000801     C* Ebbene...
047700000801     C                   ADD       1             §CTRMO
047800010201     C                   IF        errore <> *zeros
047900000801     C                   ADD       1             §CTRNO
048000000801     C                   EVAL      recko = vindta
048100000801     C                   EXCEPT    rigadett
048200000801     C                   EVAL      vinflg = '2'
048300000801     C                   ELSE
048400010201     C                   ADD       1             §CTROKVB
048500000801     C                   ENDIF
048600000801     C*
048700000801     C                   ENDSR
048800070102     C*----------------------------------------------------*
048900091223     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
049000070102     C*----------------------------------------------------*
049100070103     C     EXEVATE       BEGSR
049200070102     C*
049300091223     C***                EVAL      VATTRC = 'E'
049400091223     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
049500090428     C*
049600091223     C***                exsr      wrivat                                       => scarico VAT
049700070102     C*
049800070102     C                   ENDSR
049900080415     C*----------------------------------------------------*
050000091223     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
050100080415     C*----------------------------------------------------*
050200080415     C     EXEVATA       BEGSR
050300080415     C*
050400091214     C***                EVAL      VATTRC = 'A'
050500091214     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
050600080415     C*
050700091215     C***                exsr      wrivat                                       => scarico VAT
050800080415     C*
050900080415     C                   ENDSR
051000071121     C*----------------------------------------------------*
051100091223     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
051200071121     C*----------------------------------------------------*
051300071121     C     EXEVATB       BEGSR
051400071121     C*
051500091223     C***                EVAL      VATTRC = 'B'
051600091223     C***                EVAL      VATNOT = %trim(%subst(vindta:374:30))
051700071121     C*
051800091223     C***                exsr      wrivat                                       => scarico VAT
051900071121     C*
052000071121     C                   ENDSR
052100010201     C*----------------------------------------------------*
052200091223     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
052300010201     C*----------------------------------------------------*
052400020305     C     WRIVAT        BEGSR
052500050628     C*
052600060223     C* Scrivo solo se valorizzato qualcosa
052700060223     C                   IF        VATNOT <> *blanks
052800091223     C                   WRITE     EDIVAT00
052900090922     C                   ADD       1             §ctrokvt
053000060223     C                   ENDIF
053100010201     C*
053200010201     C                   ENDSR
053300010202     C*----------------------------------------------------*
053400091223     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053500010202     C*----------------------------------------------------*
053600020305     C     PREVAT        BEGSR
053700010202     C*
053800091223     C* Compongo il nome del membro da dare al EDIVATWR
053900010202     C                   eval      parmbr = vlrhdl
054000010202     C                   movel     'M'           parmbr
054100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054200010202     C                   eval      paropz = '1'
054300010202     C* Effettuo la chiamata al CLLE preposto
054400091223     C                   call(e)   'TITVEVTC'
054500010202     C                   parm                    parccm
054600010202     C                   parm                    parmbr
054700010202     C                   parm                    paropz
054800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054900010202     C                   if        %error
055000010202     C                   movel     '1'           chkcall
055100010202     C                   else
055200010202     C                   movel     '0'           chkcall
055300010202     C                   endif
055400010202     C*
055500010202     C                   ENDSR
055600000801     C*----------------------------------------------------*
055700000801     C*  CONTROLLO NUMERICITA' CAMPI
055800000801     C*----------------------------------------------------*
055900000801     C     CHKNUM        BEGSR
056000081217     C*
056100081217     C                   IF        PiDecChr = *blanks
056200091223     C                   EVAL      PiDecChr = ','
056300081217     C                   ENDIF
056400081217     C*
056500081217     C                   callp(e)  UBISNUM_Check(PiStr
056600081217     C                                          :PiDecChr
056700081217     C                                          :PiVal
056800081217     C                                          :PiNum
056900081217     C                                          :PiInt)
057000081217     C*
057100081217     C                   IF        %error
057200081217     C                   EVAL      PiInt=*off
057300081217     C                   ENDIF
057400000801     C*
057500000801     C                   ENDSR
057600000801     C***
057700000801
057800011113
057900011113     C*----------------------------------------------------*
058000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058100011113     C*----------------------------------------------------*
058200011113     C     CHKIMPDIV     BEGSR
058300011113     C*
058400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058500011113     C                   Z-ADD     *zeros        wrkDec            9 9
058600011113     C*
058700011113     C* Come prima cosa effettuo considerazioni sulla divisa
058800011113     C                   IF        vabIAS > *zeros
058900011113     C                   IF        vabVAS <> 'EUR'
059000011113     C                   EVAL      vabVAS =  'ITL'
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   IF        vabCAS > *zeros
059500011113     C                   IF        vabVCA <> 'EUR'
059600011113     C                   EVAL      vabVCA =  'ITL'
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C                   IF        vabVMD > *zeros
060100020305     C                   IF        vabVAD <> 'EUR'
060200011113     C                   EVAL      vabVAD =  'ITL'
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060700011113     C                   Z-ADD     vabIAS        wrkDec
060800011113     C                   IF        wrkDec > *zeros
060900011113     C                   IF        vabVAS = 'ITL'
061000011113     C                   EVAL      vabIAS = *zeros
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061500011113     C                   Z-ADD     vabCAS        wrkDec
061600011113     C                   IF        wrkDec > *zeros
061700011113     C                   IF        vabVCA = 'ITL'
061800011113     C                   EVAL      vabCAS = *zeros
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062300011113     C                   Z-ADD     vabVMD        wrkDec
062400011113     C                   IF        wrkDec > *zeros
062500011113     C                   IF        vabVAD = 'ITL'
062600011113     C                   EVAL      vabVMD = *zeros
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C                   ENDSR
063100011113     C***
063200011113
063300011113
063400000801
063500000801
063600990920      /TITLE Invio dei dati al punto operativo.
063700010202     C     invio         BEGSR
063800990920     C*
063900091223     C* 1° invio EDIVAT
064000010201     C                   reset                   dscmz
064100010201     C                   move      vlrpoi        cmzdst
064200091223     C                   eval      cmzfld = 'EDIVATWR'
064300010201     C                   eval      cmzmbd = vlrhdl
064400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064500021009     C***                if        prmfir = *blanks
064600091223     C                   eval      cmzfla = 'EDIVAT0F'
064700091223     C                   eval      cmzmba = 'EDIVAT0F'
064800021009     C***                else
064900021009     C***                eval      cmzfla = prmfir
065000021009     C***                eval      cmzmba = prmfir
065100021009     C***                endif
065200010201     C                   eval      cmznrr = *zeros
065300020305     C                   move      §ctrokvt      cmznrr
065400021018     C                   eval      cmzlba = vlrfl1
065500010201     C                   call(e)   'TIS711C'
065600010201     C                   parm                    dscmz
065700010201     C                   parm      *blanks       esito
065800010205     C                   if        %error
065900010205     C                             or cmzerr = '1'
066000010205     C                             or esito  = '1'
066100010205     C                   eval      wrkesito = '3'
066200010205     C                   else
066300010201     C*
066400091223     C* 2° invio EDIVAB
066500010201     C                   reset                   dscmz
066600010201     C                   move      vlrpoi        cmzdst
066700010201     C                   eval      cmzfld = vlrfou
066800010201     C                   eval      cmzmbd = vlrhdl
066900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067000021009     C***                if        prmfir = *blanks
067100091223     C                   eval      cmzfla = 'EDIVAB0F'
067200091223     C                   eval      cmzmba = 'EDIVAB0F'
067300021009     C***                else
067400021009     C***                eval      cmzfla = prmfir
067500021009     C***                eval      cmzmba = prmfir
067600021009     C***                endif
067700010201     C                   eval      cmznrr = *zeros
067800010201     C                   move      §ctrokvb      cmznrr
067900021018     C                   eval      cmzlba = vlrfl1
068000010201     C                   call(e)   'TIS711C'
068100010201     C                   parm                    dscmz
068200010201     C                   parm      *blanks       esito
068300010201     C                   if        %error
068400010201     C                             or cmzerr = '1'
068500010201     C                             or esito  = '1'
068600010201     C                   eval      wrkesito = '3'
068700010201     C                   endif
068800010205     C                   endif
068900990920     C*
069000000613     C                   ENDSR
069100000613     C***
069200070411
069300090331
069400090331
069500090331
069600070411     C     *pssr         BEGSR
069700070411     C*
069800070411     C                   if        %open(tivin00r)
069900070411     C                   close     tivin00r
070000070411     C                   endif
070100091223     C                   if        %open(edivabwr)
070200091223     C                   close     edivabwr
070300070411     C                   endif
070400091223     C                   if        %open(edivatwr)
070500091223     C                   close     edivatwr
070600070411     C                   endif
070700070411     C*
070800070411     C* Effettuo la chiamata al CLLE preposto
070900091223     C                   call(e)   'TITVEVTC'
071000070411     C                   parm                    parccm
071100070411     C                   parm                    parmbr
071200070411     C                   parm      '2'           paropz
071300070411     C*
071400070411     C                   eval      wrkesito = '2'
071500070411     C*
071600070411     C                   seton                                        LR
071700070411     C*
071800070411     C                   ENDSR     '*CANCL'
071900070411     C***
072000070411
072100091223
072200091223     C*--------------------------------------------------------
072300091223     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
072400091223     C*--------------------------------------------------------
072500091223     C     CARTAB        BEGSR
072600091223     C*
072700091223     C* TABELLA '15' - NAZIONI
072800091223     C                   clear                   skNAZISO
072900091223     C                   clear                   skNAZBAR
073000091223     C                   eval      tblKUT = 1
073100091223     C                   eval      tblCOD = '15'
073200091223     C     KEYtabP       setll     tabel00f
073300091223     C     KEYtabP       reade     tabel00f
073400091223     C                   dow       not %eof(tabel00f)
073500091223     C                   if        tblFLG = *blanks
073600091223     C                   movel(p)  tblUNI        ds15
073700091223     C                   add       1             jNAZ
073800091223     C                   eval      skNAZBAR(jNAZ) = tblKEY
073900091223     C                   eval      skNAZISO(jNAZ) = §15COD
074000091223     C                   endif
074100091223     C     KEYtabP       reade     tabel00f
074200091223     C                   enddo
074300091223     C*
074400091223     C                   ENDSR
074500091223     C***
074600090331
074700090331
074800990910
074900000613     C     *inzsr        BEGSR
075000990910     C*
075100990910     C     *entry        plist
075200990920     C                   parm                    tivlrds
075300990921     C                   parm      wrkesito      esito
075400000724     C                   parm                    prmlit
075500000710     C                   parm                    prmfir
075600091223     C*
075700091223     C* Chiave su TABEL00F - parziale
075800091223     C     KEYtabP       KLIST
075900091223     C                   KFLD                    tblKUT
076000091223     C                   KFLD                    tblCOD
076100000613     C*
076200091223     C                   MOVE(P)   vlrMSG        wNomeFile
076300091223     C*
076400091223     C                   time                    wn14             14 0
076500091223     C                   movel     wn14          oracor            6 0          *ORA
076600091214     C                   z-add     *zeros        datcor            8 0
076700091214     C                   eval      datcor = %dec(%date() : *ISO)
076800000830     C*
076900000613     C                   ENDSR
077000000613     C***
077100990908
077200090331
077300090331
077400091223     Otitv1u0p  E            riepilogo         2
077500990915     O                                              'Upload via Internet'
077600990915     O                                           +1 'Traduzione TIVIN00R -'
077700091223     O                                           +1 'EDIVABWR/EDIVATWR'
077800010201     O                                           +1 'Report testate bolle'
077900990915     O          E            riepilogo   2
078000990915     O                       wrkdata
078100990915     O                       wrkora              +1
078200990915     O                       procname            +1
078300990915     O          E            riepilogo   2
078400990915     O                                              'Cliente..................:'
078500990915     O                       VABCCM        z     +1
078600990915     O          E            riepilogo   2
078700990920     O                                              'Riferimento Strategi.....:'
078800990920     O                       vlrhdl              +1
078900990915     O          E            riepilogo   2
079000990915     O                                              'Giusti...................:'
079100010201     O                       §CTROKVB      2   +  1
079200990915     O          E            riepilogo   2
079300990915     O                                              'Sbagliati e corretti.....:'
079400971022     O                       §CTRMO        2   +  1
079500990915     O          E            riepilogo   2
079600990915     O                                              'Sbagliati e scartati.....:'
079700971022     O                       §CTRNO        2   +  1
079800000613
079900091223     Otitv1u0ps E            testdett          2
080000000613     O                                              'Upload via Internet'
080100000613     O                                           +1 'Traduzione TIVIN00R -'
080200091223     O                                           +1 'EDIVABWR/EDIVATWR'
080300010201     O                                           +1 'Report testate bolle'
080400000616     O          E            testdett    3
080500000613     O                                           +2 'N° rec'
080600000613     O                                           +3 'Anteprima contenuto'
080700000616     O          E            rigadett    2
080800000613     O                       rrnum               +2
080900000621     O                       recko               +3
081000000616     O          E            findett     2
081100000613     O                       wrkdata
081200000613     O                       wrkora              +1
081300000613     O                       procname            +1
081400000616     O          E            findett     2
081500000613     O                                              'Cliente..................:'
081600000613     O                       VABCCM        z     +1
081700000616     O          E            findett     2
081800000613     O                                              'Riferimento Strategi.....:'
081900000613     O                       vlrhdl              +1
082000000616     O          E            findett     2
082100000613     O                                              'Giusti...................:'
082200010201     O                       §CTROKVB      2   +  1
082300000616     O          E            findett     2
082400000613     O                                              'Sbagliati e corretti.....:'
082500000613     O                       §CTRMO        2   +  1
082600000616     O          E            findett     2
082700000613     O                                              'Sbagliati e scartati.....:'
082800000613     O                       §CTRNO        2   +  1
082900000512** CMD - COMANDI CL
083000091223OVRPRTF FILE(TITV1U0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
083100091223OVRPRTF FILE(TITV1U0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
083200091223DLTOVR FILE(TITV1U0P TITV1U0PS) LVL(*)
083300000512
083400000512
