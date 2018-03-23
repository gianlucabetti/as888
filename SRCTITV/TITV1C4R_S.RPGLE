000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800071121     Ftitv1c4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100071121     Ftitv1c4ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000070927     D curSped         s              7    INZ(*blanks)
004100070927     D depSped         s              7    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100010201
005200010201
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000000724     C                   if        vlrpoi <> *zeros
006100000724     C*
006200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000724     C     vlrpoi        chain     azorg01l
006500000724     C                   if        %found
006600000616     C                   movel(p)  CMD(1)        depcmd
006700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006800000616     C*
006900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007000011113     C                   Z-ADD     150           LENGH            15 5
007100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200000616     C                   PARM                    depcmd
007300000616     C                   PARM                    LENGH
007400000724     C*
007500000724     C                   endif
007600000724     C                   endif
007700000616     C*
007800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000616     C                   movel(p)  CMD(2)        depcmd
008000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000616     C*
008800071121     C                   if        not %open(titv1c4ps)
008900071121     C                   open      titv1c4ps
009000000616     C                   except    testdett
009100000613     C                   endif
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009500000613     C*
009600010202     C* Effettuo la chiamata al CLLE preposto
009700040506     C                   call(e)   'TITVVTC'
009800010202     C                   parm                    parccm
009900010202     C                   parm                    parmbr
010000010202     C                   parm      '2'           paropz
010100050201     C*
010200050201     C* Effettuo lancio TISI95 solo x chiusura
010300050201     C                   CLEAR                   TISI95DS
010400050201     C                   EVAL      I95TLA = 'C'
010500050201     C                   CALL      'TISI95R'
010600050201     C                   PARM                    TISI95DS
010700010202     C*
010800071121     C                   if        %open(titv1c4ps)
010900000616     C                   except    findett
011000071121     C                   close     titv1c4ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900000801     C
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800071121     C                   if        not %open(titv1c4p)
012900071121     C                   open      titv1c4p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400071121     C                   if        %open(titv1c4p)
013500071121     C                   close     titv1c4p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014400910830     C*--------------------------------------------------------
014500040526     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021113     C                   if        not %open(fivabwwr)
015100021113     C                   open      fivabwwr
015200990914     C                   endif
015300070103     C*
015400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015500020305     C                   exsr      prevat
015600010201     C*
015700010202     C                   if        chkcall = '0'
015800010202     C*
015900021113     C                   if        not %open(fivatwwr)
016000021113     C                   open      fivatwwr
016100010201     C                   endif
016200990910     C*
016300010201     C                   clear                   §CTROKVB          5 0
016400020305     C                   clear                   §CTROKVT          5 0
016500000801     C                   clear                   §CTRMO            5 0
016600000801     C                   clear                   §CTRNO            5 0
016700990910     C*
016800921023     C                   DO        *HIVAL
016900990913     C*
017000990915     C                   READ      tivin00r                               70
017100050627     C                   if        vindta > *blanks
017200000613     C                   add       1             rrnum
017300000801     C*
017400000801     C                   if        *in70 = *off
017500000801     C                             and
017600000801     C                             (vinflg = *blanks
017700000801     C                              or vinflg = '0'
017800000801     C                              or vinflg = '2')
017900000801     C*
018000000801     C                   clear                   vinmsg
018100000801     C                   eval      vinflg = '1'
018200070103     C*
018300070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018400071121     C                   eval      curSped = %subst(vindta:11:7)
018500070213     C                   if        curSped <> depSped
018600070213     C* Se prima bolla => importo bolla corrente
018700070213     C                   if        §CTRMO = *zeros
018800070213     C                   exsr      inzvar
018900070213     C                   exsr      defcam
019000070213     C                   exsr      impvab                                       => carico  VAB
019100070213     C                   else
019200070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019300070213     C                   exsr      wrivab                                       => scarico VAB
019400070213     C                   exsr      inzvar
019500070213     C                   exsr      defcam
019600070213     C                   exsr      impvab                                       => carico  VAB
019700070213     C                   endif
019800071121     C                   exsr      exevate                                      => write VAT-E
019900070103     C* Salvo il raggruppamento spedizione cliente corrente
020000070103     C                   eval      depSped = curSped
020100070213     C*
020200070213     C* Se collo successivo x stessa bolla
020300070213     C                   else
020400070213     C                   exsr      impvab                                       => carico  VAB
020500070213     C                   exsr      exevate                                      => write VAT-E
020600070103     C                   endif
020700000905     C*
020800000905     C                   else
020900000905     C                   eval      vinflg = '1'
021000050628     C                   endif
021100000905     C                   endif
021200000905     C*
021300000905     C  N70              update    tivin000
021400000905     C*
021500991022     C  N70              ENDdo
021600070213     C*
021700070213     C* Scarico testata bolla rimasta in sospesa
021800070213     C                   exsr      wrivab                                       => scarico VAB
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021113     C                   if        %open(fivabwwr)
023800021113     C                   close     fivabwwr
023900990914     C                   endif
024000021113     C                   if        %open(fivatwwr)
024100021113     C                   close     fivatwwr
024200010201     C                   endif
024300990910     C*
024400010201     C                   if        §ctrokvb > 0
024500000724     C                             and vlrpoi <> *zeros
024600010202     C                   exsr      invio
024700990920     C                   endif
024800990920     C*
024900910830     C                   ENDSR
025000000613     C***
025100010305
025200010305     C*----------------------------------------------------*
025300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025400010305     C*----------------------------------------------------*
025500020305     C     WRIVAB        BEGSR
025600010305     C*
025700060225     C* Quindi scarico il buffer del file d testata
025800021113     C                   write     fivab000                                     => scarico il VAB
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700070213     C                   CLEAR                   FIVAB000
026800070213     C                   CLEAR                   FIVAT000
026900070213     C*
027000040802     C                   Z-ADD     *zeros        Num5_0            5 0
027100040802     C                   MOVEL     '0'           FlgCAS            1
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700000801     C     DEFCAM        BEGSR
027800000801     C*
027900020619     C* Imposto i valori di default...
028000071121     C                   Z-ADD     0054370       VABCCM
028100071121     C                   Z-ADD     0054370       VATCCM
028200071121     C                   Z-ADD     005           VABLNP
028300071121     C                   Z-ADD     005           VATLNP
028400070927     C                   Z-ADD     000           VABCTR
028500070103     C                   MOVEL     '7Q'          VABCTM
028600040823     C                   MOVEL     '1'           VABCBO
028700020619     C* ... e poi verifico se sono stati passati come parametri
028800020619     C                   IF        vlrppt > *blanks
028900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABCCM
029400020619     C                   Z-ADD     PiVal         VATCCM
029500020619     C                   ENDIF
029600040506     C                   ENDIF
029700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029900020619     C                   EXSR      CHKNUM
030000020619     C                   IF        PiInt=*on
030100020619     C                   Z-ADD     PiVal         VABLNP
030200020619     C                   Z-ADD     PiVal         VATLNP
030300040506     C                   ENDIF
030400020619     C                   ENDIF
030500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030700020619     C                   EXSR      CHKNUM
030800020619     C                   IF        PiInt=*on
030900020619     C                   Z-ADD     PiVal         VABCTR
031000040506     C                   ENDIF
031100020619     C                   ENDIF
031200020619     C                   ENDIF
031300000801     C*
031400000801     C                   ENDSR
031500000801     C*----------------------------------------------------*
031600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031700000801     C*----------------------------------------------------*
031800040823     C     IMPVAB        BEGSR
031900010305     C*
032000000801     C                   Z-ADD     *zeros        errore            1 0
032100000830     C                   MOVEL     datcor        VABAAS
032200020305     C                   MOVEL     datcor        VATAAS
032300040526     C                   MOVE      datcor        VABMGS
032400040823     C                   MOVE(P)   vlrpoi        VABFGS
032500040823     C                   MOVE(P)   vlrpoi        VATFGS
032600050628     C*
032700071121     C                   EVAL      VABRSD=%trim(%subst(vindta:18:35))
032800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033000020117     C* ==
033100071121     C                   EVAL      VABRD2=%trim(%subst(vindta:53:35))
033200071121     C                   EVAL      VABIND=%trim(%subst(vindta:88:35))
033300071121     C                   EVAL      VABLOD=%trim(%subst(vindta:132:35))
033400071121     C                   EVAL      VABPRD=%trim(%subst(vindta:167:2))
033500071121     C                   EVAL      VABNZD=%trim(%subst(vindta:169:3))
033600071121     C                   EVAL      VABNAS=%trim(%subst(vindta:175:15))
033700071121     C                   EVAL      VABRMA=%trim(%subst(vindta:219:15))
033800071121     C                   EVAL      VABNT2=%trim(%subst(vindta:234:35))
033900050628     C* CAP
034000071121     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
034100010201     C                   EXSR      CHKNUM
034200010201     C                   IF        PiInt=*on
034300010201     C                   Z-ADD     PiVal         Num5_0
034400040506     C                   MOVEL(P)  Num5_0        VABCAD
034500010201     C                   ELSE
034600040506     C                   ADD       1             errore
034700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034800040506     C                             + ' ' + 'VABCAD'
034900010201     C                   ENDIF
035000040506     C* Reperisco la provincia dal CAP e dalla località
035100040526     C                   IF        VABCAD <> *blanks AND
035200040526     C                             VABPRD  = *blanks
035300040506     C                   CLEAR                   TISI95DS
035400040506     C                   EVAL      I95TCN = '3'
035500040506     C                   Z-ADD     datcor        I95DAT
035600040506     C                   EVAL      I95CAP = VABCAD
035700040506     C                   EVAL      I95LOC = VABLOD
035800050627     C                   EVAL      I95NAR = VABNZD
035900040506     C                   CALL      'TISI95R'
036000040506     C                   PARM                    TISI95DS
036100040506     C                   EVAL      VABPRD = O95PRV
036200040506     C                   ENDIF
036300071121     C* CTR
036400071121     C                   EVAL      PiStr=%trim(%subst(vindta:172:3))
036500071121     C                   EXSR      CHKNUM
036600071121     C                   IF        PiInt=*on
036700071121     C                   Z-ADD     PiVal         VABCTR
036800071121     C                   ELSE
036900071121     C                   ADD       1             errore
037000071121     C                   EVAL      vinmsg = %trimr(vinmsg)
037100071121     C                             + ' ' + 'VABCTR'
037200071121     C                   ENDIF
037300060307     C* NCL
037400071121     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
037500071121     C                   EXSR      CHKNUM
037600071121     C                   IF        PiInt=*on
037700071121     C                   Z-ADD     PiVal         VABNCL
037800071121     C                   ELSE
037900071121     C                   ADD       1             errore
038000071121     C                   EVAL      vinmsg = %trimr(vinmsg)
038100071121     C                             + ' ' + 'VABNCL'
038200071121     C                   ENDIF
038300040506     C* PKB
038400071123     C                   EVAL      PiStr=%trim(%subst(vindta:196:8))
038500010201     C                   EXSR      CHKNUM
038600010201     C                   IF        PiNum=*on
038700071121     C                   Z-ADD     PiVal         VABPKB
038800010201     C                   ELSE
038900010201     C                   ADD       1             errore
039000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039100010201     C                             + ' ' + 'VABPKB'
039200010201     C                   ENDIF
039300071121     C* RMN
039400071121     C                   EVAL      PiStr=%trim(%subst(vindta:204:15))
039500071121     C                   EXSR      CHKNUM
039600071121     C                   IF        PiInt=*on
039700071121     C                   Z-ADD     PiVal         VABRMN
039800071121     C                   ELSE
039900071121     C                   ADD       1             errore
040000071121     C                   EVAL      vinmsg = %trimr(vinmsg)
040100071121     C                             + ' ' + 'VABRMN'
040200071121     C                   ENDIF
040300071121     C* NSP
040400071121     C                   EVAL      PiStr=%trim(%subst(vindta:11:7))
040500071019     C                   EXSR      CHKNUM
040600071019     C                   IF        PiInt=*on
040700071019     C                   Z-ADD     PiVal         VABNSP
040800071019     C                   Z-ADD     PiVal         VATNSP
040900071019     C                   ELSE
041000071019     C                   ADD       1             errore
041100071019     C                   EVAL      vinmsg = %trimr(vinmsg)
041200071121     C                             + ' ' + 'VABNSP VATNSP'
041300071019     C                   ENDIF
041400071121     C* LNP
041500071121     C                   EVAL      PiStr=%trim(%subst(vindta:8:3))
041600071121     C                   EXSR      CHKNUM
041700071121     C                   IF        PiInt=*on
041800071121     C                   Z-ADD     PiVal         VABLNP
041900071121     C                   Z-ADD     PiVal         VATLNP
042000071121     C                   ELSE
042100071121     C                   ADD       1             errore
042200071121     C                   EVAL      vinmsg = %trimr(vinmsg)
042300071121     C                             + ' ' + 'VABLNP VATLNP'
042400071121     C                   ENDIF
042500071121     C*
042600071121     C* Carico l'estensione B del FNVAT
042700071121     C                   exsr      exevatb
042800010205     C*
042900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043000040802     C                   IF        FlgCAS <> '0'
043100070102     C                   IF        VABCBO = '1'
043200010205     C                   EVAL      VABCBO = '4'
043300010205     C                   ELSE
043400070102     C                   EVAL      VABCBO = '6'
043500070102     C                   ENDIF
043600010205     C                   ENDIF
043700020305     C*
043800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900011113     C                   EXSR      CHKIMPDIV
044000010202     C*
044100000801     C* Ebbene...
044200000801     C                   ADD       1             §CTRMO
044300010201     C                   IF        errore <> *zeros
044400000801     C                   ADD       1             §CTRNO
044500000801     C                   EVAL      recko = vindta
044600000801     C                   EXCEPT    rigadett
044700000801     C                   EVAL      vinflg = '2'
044800000801     C                   ELSE
044900010201     C                   ADD       1             §CTROKVB
045000000801     C                   ENDIF
045100000801     C*
045200000801     C                   ENDSR
045300070102     C*----------------------------------------------------*
045400070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045500070102     C*----------------------------------------------------*
045600070103     C     EXEVATE       BEGSR
045700070102     C*
045800070103     C                   EVAL      VATTRC='E'
045900071121     C                   EVAL      VATNOT = %trim(%subst(vindta:219:15))
046000071019     C*
046100070102     C                   exsr      wrivat                                       => scarico VAT
046200070102     C*
046300070102     C                   ENDSR
046400071121     C*----------------------------------------------------*
046500071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046600071121     C*----------------------------------------------------*
046700071121     C     EXEVATB       BEGSR
046800071121     C*
046900071121     C                   EVAL      VATTRC='B'
047000071121     C                   EVAL      VATNOT = %trim(%subst(vindta:269:35))
047100071121     C* Se nn passato imposto x default il '.'
047200071121     C                   IF        VATNOT = *blanks
047300071121     C                   EVAL      VATNOT = '.'
047400071121     C                   ENDIF
047500071121     C*
047600071121     C                   exsr      wrivat                                       => scarico VAT
047700071121     C*
047800071121     C                   ENDSR
047900010201     C*----------------------------------------------------*
048000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048100010201     C*----------------------------------------------------*
048200020305     C     WRIVAT        BEGSR
048300050628     C*
048400060223     C* Scrivo solo se valorizzato qualcosa
048500060223     C                   IF        VATNOT <> *blanks
048600040802     C                   WRITE     FIVAT000
048700060223     C                   ENDIF
048800010201     C*
048900010201     C                   ENDSR
049000010202     C*----------------------------------------------------*
049100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049200010202     C*----------------------------------------------------*
049300020305     C     PREVAT        BEGSR
049400010202     C*
049500021113     C* Compongo il nome del membro da dare al FIVATWWR
049600010202     C                   eval      parmbr = vlrhdl
049700010202     C                   movel     'M'           parmbr
049800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049900010202     C                   eval      paropz = '1'
050000010202     C* Effettuo la chiamata al CLLE preposto
050100040506     C                   call(e)   'TITVVTC'
050200010202     C                   parm                    parccm
050300010202     C                   parm                    parmbr
050400010202     C                   parm                    paropz
050500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600010202     C                   if        %error
050700010202     C                   movel     '1'           chkcall
050800010202     C                   else
050900010202     C                   movel     '0'           chkcall
051000010202     C                   endif
051100010202     C*
051200010202     C                   ENDSR
051300000801     C*----------------------------------------------------*
051400000801     C*  CONTROLLO NUMERICITA' CAMPI
051500000801     C*----------------------------------------------------*
051600000801     C     CHKNUM        BEGSR
051700000801     C*
051800000801     C                   call(e)   'ISNUMERIC'
051900000801     C                   PARM                    PiStr            30
052000070213     C                   PARM      ','           PiDecChr          1
052100000801     C                   PARM      *ZEROS        PiVal            30 9
052200000801     C                   PARM      '0'           PiInt             1
052300000801     C                   PARM      '0'           PiNum             1
052400000801     C                   IF        %error
052500000801     C                   EVAL      PiInt=*off
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900000801     C***
053000000801
053100011113
053200011113     C*----------------------------------------------------*
053300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053400011113     C*----------------------------------------------------*
053500011113     C     CHKIMPDIV     BEGSR
053600011113     C*
053700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053800011113     C                   Z-ADD     *zeros        wrkDec            9 9
053900011113     C*
054000011113     C* Come prima cosa effettuo considerazioni sulla divisa
054100011113     C                   IF        vabIAS > *zeros
054200011113     C                   IF        vabVAS <> 'EUR'
054300011113     C                   EVAL      vabVAS =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabCAS > *zeros
054800011113     C                   IF        vabVCA <> 'EUR'
054900011113     C                   EVAL      vabVCA =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabVMD > *zeros
055400020305     C                   IF        vabVAD <> 'EUR'
055500011113     C                   EVAL      vabVAD =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056000011113     C                   Z-ADD     vabIAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAS = 'ITL'
056300011113     C                   EVAL      vabIAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056800011113     C                   Z-ADD     vabCAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVCA = 'ITL'
057100011113     C                   EVAL      vabCAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057600011113     C                   Z-ADD     vabVMD        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAD = 'ITL'
057900011113     C                   EVAL      vabVMD = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   ENDSR
058400011113     C***
058500011113
058600011113
058700000801
058800000801
058900990920      /TITLE Invio dei dati al punto operativo.
059000010202     C     invio         BEGSR
059100990920     C*
059200021113     C* 1° invio FIVAT
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500021113     C                   eval      cmzfld = 'FIVATWWR'
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAT00F'
060000021113     C                   eval      cmzmba = 'FIVAT00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600020305     C                   move      §ctrokvt      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010205     C                   if        %error
061200010205     C                             or cmzerr = '1'
061300010205     C                             or esito  = '1'
061400010205     C                   eval      wrkesito = '3'
061500010205     C                   else
061600010201     C*
061700021113     C* 2° invio FIVAB
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000010201     C                   eval      cmzfld = vlrfou
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400021113     C                   eval      cmzfla = 'FIVAB00F'
062500021113     C                   eval      cmzmba = 'FIVAB00F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100010201     C                   move      §ctrokvb      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010201     C                   if        %error
063700010201     C                             or cmzerr = '1'
063800010201     C                             or esito  = '1'
063900010201     C                   eval      wrkesito = '3'
064000010201     C                   endif
064100010205     C                   endif
064200990920     C*
064300000613     C                   ENDSR
064400000613     C***
064500070411
064600070411     C     *pssr         BEGSR
064700070411     C*
064800070411     C                   if        %open(tivin00r)
064900070411     C                   close     tivin00r
065000070411     C                   endif
065100070411     C                   if        %open(fivabwwr)
065200070411     C                   close     fivabwwr
065300070411     C                   endif
065400070411     C                   if        %open(fivatwwr)
065500070411     C                   close     fivatwwr
065600070411     C                   endif
065700070411     C*
065800070411     C* Effettuo la chiamata al CLLE preposto
065900070411     C                   call(e)   'TITVVTC'
066000070411     C                   parm                    parccm
066100070411     C                   parm                    parmbr
066200070411     C                   parm      '2'           paropz
066300070411     C*
066400070411     C                   eval      wrkesito = '2'
066500070411     C*
066600070411     C                   seton                                        LR
066700070411     C*
066800070411     C                   ENDSR     '*CANCL'
066900070411     C***
067000070411
067100990910
067200000613     C     *inzsr        BEGSR
067300990910     C*
067400990910     C     *entry        plist
067500990920     C                   parm                    tivlrds
067600990921     C                   parm      wrkesito      esito
067700000724     C                   parm                    prmlit
067800000710     C                   parm                    prmfir
067900000613     C*
068000000830     C* CALCOLA LA DATA CORRENTE
068100000830     C                   time                    wn14             14 0
068200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068300000830     C                   z-add     wn8           g08dat
068400000830     C                   z-add     *zeros        g08inv
068500000830     C                   movel     '0'           g08err
068600000830     C                   call      'XSRDA8'
068700000830     C                   parm                    wlbda8
068800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068900000830     C*
069000000613     C                   ENDSR
069100000613     C***
069200990908
069300071121     Otitv1c4p  E            riepilogo         2
069400990915     O                                              'Upload via Internet'
069500990915     O                                           +1 'Traduzione TIVIN00R -'
069600021113     O                                           +1 'FIVABWWR/FIVATWWR'
069700010201     O                                           +1 'Report testate bolle'
069800990915     O          E            riepilogo   2
069900990915     O                       wrkdata
070000990915     O                       wrkora              +1
070100990915     O                       procname            +1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Cliente..................:'
070400990915     O                       VABCCM        z     +1
070500990915     O          E            riepilogo   2
070600990920     O                                              'Riferimento Strategi.....:'
070700990920     O                       vlrhdl              +1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Giusti...................:'
071000010201     O                       §CTROKVB      2   +  1
071100990915     O          E            riepilogo   2
071200990915     O                                              'Sbagliati e corretti.....:'
071300971022     O                       §CTRMO        2   +  1
071400990915     O          E            riepilogo   2
071500990915     O                                              'Sbagliati e scartati.....:'
071600971022     O                       §CTRNO        2   +  1
071700000613
071800071121     Otitv1c4ps E            testdett          2
071900000613     O                                              'Upload via Internet'
072000000613     O                                           +1 'Traduzione TIVIN00R -'
072100021113     O                                           +1 'FIVABWWR/FIVATWWR'
072200010201     O                                           +1 'Report testate bolle'
072300000616     O          E            testdett    3
072400000613     O                                           +2 'N° rec'
072500000613     O                                           +3 'Anteprima contenuto'
072600000616     O          E            rigadett    2
072700000613     O                       rrnum               +2
072800000621     O                       recko               +3
072900000616     O          E            findett     2
073000000613     O                       wrkdata
073100000613     O                       wrkora              +1
073200000613     O                       procname            +1
073300000616     O          E            findett     2
073400000613     O                                              'Cliente..................:'
073500000613     O                       VABCCM        z     +1
073600000616     O          E            findett     2
073700000613     O                                              'Riferimento Strategi.....:'
073800000613     O                       vlrhdl              +1
073900000616     O          E            findett     2
074000000613     O                                              'Giusti...................:'
074100010201     O                       §CTROKVB      2   +  1
074200000616     O          E            findett     2
074300000613     O                                              'Sbagliati e corretti.....:'
074400000613     O                       §CTRMO        2   +  1
074500000616     O          E            findett     2
074600000613     O                                              'Sbagliati e scartati.....:'
074700000613     O                       §CTRNO        2   +  1
074800000512** CMD - COMANDI CL
074900071121OVRPRTF FILE(TITV1C4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075000071121OVRPRTF FILE(TITV1C4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075100071121DLTOVR FILE(TITV1C4P TITV1C4PS) LVL(*)
075200000512
075300000512
