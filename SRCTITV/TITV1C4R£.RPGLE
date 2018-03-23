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
005800081106     C*
005900081106     C* SALVO IL P.O. INVIO INIZIALE
006000081106     C                   z-add     vlrpoi        sv_vlrpoi         3 0
006100000724     C*
006200000724     C* SE OCCORRE SPEDIRE IN FILIALE
006300081106     C                   if        sv_vlrpoi <> *zeros AND
006400081106     C                             sv_vlrpoi <> 999
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800000724     C     vlrpoi        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011113     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200071121     C                   if        not %open(titv1c4ps)
009300071121     C                   open      titv1c4ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009800081106     C*
009900081106     C                   if        sv_vlrpoi <> *zeros AND
010000081106     C                             sv_vlrpoi <> 999
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010200081106     C                   endif
010300000613     C*
010400010202     C* Effettuo la chiamata al CLLE preposto
010500040506     C                   call(e)   'TITVVTC'
010600010202     C                   parm                    parccm
010700010202     C                   parm                    parmbr
010800010202     C                   parm      '2'           paropz
010900050201     C*
011000050201     C* Effettuo lancio TISI95 solo x chiusura
011100050201     C                   CLEAR                   TISI95DS
011200050201     C                   EVAL      I95TLA = 'C'
011300050201     C                   CALL      'TISI95R'
011400050201     C                   PARM                    TISI95DS
011500010202     C*
011600071121     C                   if        %open(titv1c4ps)
011700000616     C                   except    findett
011800071121     C                   close     titv1c4ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300081106     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700000801     C
012800010201     C                   seton                                        LR
012900000613
013000000613
013100000613     C*--------------------------------------------------------
013200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013300000613     C*--------------------------------------------------------
013400000621     C     STPR          BEGSR
013500000613     C*
013600071121     C                   if        not %open(titv1c4p)
013700071121     C                   open      titv1c4p
013800990915     C                   endif
013900990915     C*
014000990915     C                   except    riepilogo
014100990915     C*
014200071121     C                   if        %open(titv1c4p)
014300071121     C                   close     titv1c4p
014400990914     C                   endif
014500990910     C*
014600000613     C                   ENDSR
014700000613     C***
014800990908
014900000801
015000910830     C*--------------------------------------------------------
015100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015200910830     C*--------------------------------------------------------
015300040526     C     RWFILE        BEGSR
015400990910     C*
015500990914     C                   if        not %open(tivin00r)
015600990908     C                   open      tivin00r
015700990914     C                   endif
015800021113     C                   if        not %open(fivabwwr)
015900021113     C                   open      fivabwwr
016000990914     C                   endif
016100070103     C*
016200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700021113     C                   if        not %open(fivatwwr)
016800021113     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900050627     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000070103     C*
019100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019200071121     C                   eval      curSped = %subst(vindta:11:7)
019300070213     C                   if        curSped <> depSped
019400070213     C* Se prima bolla => importo bolla corrente
019500070213     C                   if        §CTRMO = *zeros
019600070213     C                   exsr      inzvar
019700070213     C                   exsr      defcam
019800070213     C                   exsr      impvab                                       => carico  VAB
019900070213     C                   else
020000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020100070213     C                   exsr      wrivab                                       => scarico VAB
020200070213     C                   exsr      inzvar
020300070213     C                   exsr      defcam
020400070213     C                   exsr      impvab                                       => carico  VAB
020500070213     C                   endif
020600071121     C                   exsr      exevate                                      => write VAT-E
020700070103     C* Salvo il raggruppamento spedizione cliente corrente
020800070103     C                   eval      depSped = curSped
020900070213     C*
021000070213     C* Se collo successivo x stessa bolla
021100070213     C                   else
021200070213     C                   exsr      impvab                                       => carico  VAB
021300070213     C                   exsr      exevate                                      => write VAT-E
021400070103     C                   endif
021500000905     C*
021600000905     C                   else
021700000905     C                   eval      vinflg = '1'
021800050628     C                   endif
021900000905     C                   endif
022000000905     C*
022100000905     C  N70              update    tivin000
022200000905     C*
022300991022     C  N70              ENDdo
022400070213     C*
022500070213     C* Scarico testata bolla rimasta in sospesa
022600070213     C                   exsr      wrivab                                       => scarico VAB
022700010202     C*
022800010202     C                   endif
022900990910
023000990910     C* Se non ci sono record con errori ...
023100000710     C                   if        §ctrno = 0
023200990910     C* ... restituisco esito OK.
023300990921     C                   eval      wrkesito = '0'
023400990910     C                   else
023500010201     C                   if        §ctrokvb > 0
023600990921     C                   eval      wrkesito = '1'
023700000710     C                   else
023800000710     C                   eval      wrkesito = '2'
023900990910     C                   endif
024000000710     C                   endif
024100990910     C*
024200990914     C                   if        %open(tivin00r)
024300990908     C                   close     tivin00r
024400990914     C                   endif
024500021113     C                   if        %open(fivabwwr)
024600021113     C                   close     fivabwwr
024700990914     C                   endif
024800021113     C                   if        %open(fivatwwr)
024900021113     C                   close     fivatwwr
025000010201     C                   endif
025100990910     C*
025200010201     C                   if        §ctrokvb > 0
025300000724     C                             and vlrpoi <> *zeros
025400010202     C                   exsr      invio
025500990920     C                   endif
025600990920     C*
025700910830     C                   ENDSR
025800000613     C***
025900010305
026000010305     C*----------------------------------------------------*
026100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026200010305     C*----------------------------------------------------*
026300020305     C     WRIVAB        BEGSR
026400010305     C*
026500060225     C* Quindi scarico il buffer del file d testata
026600021113     C                   write     fivab000                                     => scarico il VAB
026700010305     C*
026800010305     C                   ENDSR
026900990920
027000000801     C*----------------------------------------------------*
027100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027200000801     C*----------------------------------------------------*
027300010201     C     INZVAR        BEGSR
027400000801     C*
027500070213     C                   CLEAR                   FIVAB000
027600070213     C                   CLEAR                   FIVAT000
027700070213     C*
027800040802     C                   Z-ADD     *zeros        Num5_0            5 0
027900040802     C                   MOVEL     '0'           FlgCAS            1
028000000801     C*
028100000801     C                   ENDSR
028200000801     C*----------------------------------------------------*
028300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028400000801     C*----------------------------------------------------*
028500000801     C     DEFCAM        BEGSR
028600000801     C*
028700020619     C* Imposto i valori di default...
028800071121     C                   Z-ADD     0054370       VABCCM
028900071121     C                   Z-ADD     0054370       VATCCM
029000071121     C                   Z-ADD     005           VABLNP
029100071121     C                   Z-ADD     005           VATLNP
029200070927     C                   Z-ADD     000           VABCTR
029300070103     C                   MOVEL     '7Q'          VABCTM
029400040823     C                   MOVEL     '1'           VABCBO
029500020619     C* ... e poi verifico se sono stati passati come parametri
029600020619     C                   IF        vlrppt > *blanks
029700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029900020619     C                   EXSR      CHKNUM
030000020619     C                   IF        PiInt=*on
030100020619     C                   Z-ADD     PiVal         VABCCM
030200020619     C                   Z-ADD     PiVal         VATCCM
030300020619     C                   ENDIF
030400040506     C                   ENDIF
030500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030700020619     C                   EXSR      CHKNUM
030800020619     C                   IF        PiInt=*on
030900020619     C                   Z-ADD     PiVal         VABLNP
031000020619     C                   Z-ADD     PiVal         VATLNP
031100040506     C                   ENDIF
031200020619     C                   ENDIF
031300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031500020619     C                   EXSR      CHKNUM
031600020619     C                   IF        PiInt=*on
031700020619     C                   Z-ADD     PiVal         VABCTR
031800040506     C                   ENDIF
031900020619     C                   ENDIF
032000020619     C                   ENDIF
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032500000801     C*----------------------------------------------------*
032600040823     C     IMPVAB        BEGSR
032700010305     C*
032800000801     C                   Z-ADD     *zeros        errore            1 0
032900000830     C                   MOVEL     datcor        VABAAS
033000020305     C                   MOVEL     datcor        VATAAS
033100040526     C                   MOVE      datcor        VABMGS
033200040823     C                   MOVE(P)   vlrpoi        VABFGS
033300040823     C                   MOVE(P)   vlrpoi        VATFGS
033400050628     C*
033500071121     C                   EVAL      VABRSD=%trim(%subst(vindta:18:35))
033600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033800020117     C* ==
033900071121     C                   EVAL      VABRD2=%trim(%subst(vindta:53:35))
034000071121     C                   EVAL      VABIND=%trim(%subst(vindta:88:35))
034100071121     C                   EVAL      VABLOD=%trim(%subst(vindta:132:35))
034200071121     C                   EVAL      VABPRD=%trim(%subst(vindta:167:2))
034300071121     C                   EVAL      VABNZD=%trim(%subst(vindta:169:3))
034400071121     C                   EVAL      VABNAS=%trim(%subst(vindta:175:15))
034500071121     C                   EVAL      VABRMA=%trim(%subst(vindta:219:15))
034600081014     C                   EVAL      VABNOT=%trim(%subst(vindta:320:15))
034700071121     C                   EVAL      VABNT2=%trim(%subst(vindta:234:35))
034800150219     C                   IF        %trim(%subst(vindta:369:2)) = 'A'
034900150223     C                   EVAL      VABTC1 = 'A'
035000150219     C                   ENDIF
035100090716     C* CCM
035200090716     C                   IF        %subst(vindta:1:7) <> *blanks AND
035300090716     C                             %subst(vindta:1:7) <> *zeros
035400090716     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
035500090716     C                   EXSR      CHKNUM
035600090716     C                   IF        PiInt=*on
035700090716     C                   Z-ADD     PiVal         VABCCM
035800090909     C                   Z-ADD     PiVal         VATCCM
035900090716     C                   ELSE
036000090716     C                   ADD       1             errore
036100090716     C                   EVAL      vinmsg = %trimr(vinmsg)
036200090909     C                             + ' ' + 'VABCCM VATCCM'
036300090716     C                   ENDIF
036400090716     C                   ENDIF
036500090716     C*
036600090716     C                   IF        %subst(vindta:1:7) = '0054780'
036700090716     C                   EVAL      VABCCM=0054780
036800090716     C                   EVAL      VATCCM=0054780
036900090716     C                   EVAL      VABTC1='A'
037000090716     C                   ENDIF
037100050628     C* CAP
037200071121     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
037300010201     C                   EXSR      CHKNUM
037400010201     C                   IF        PiInt=*on
037500010201     C                   Z-ADD     PiVal         Num5_0
037600040506     C                   MOVEL(P)  Num5_0        VABCAD
037700010201     C                   ELSE
037800040506     C                   ADD       1             errore
037900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038000040506     C                             + ' ' + 'VABCAD'
038100010201     C                   ENDIF
038200040506     C* Reperisco la provincia dal CAP e dalla località
038300040526     C                   IF        VABCAD <> *blanks AND
038400040526     C                             VABPRD  = *blanks
038500040506     C                   CLEAR                   TISI95DS
038600040506     C                   EVAL      I95TCN = '3'
038700040506     C                   Z-ADD     datcor        I95DAT
038800040506     C                   EVAL      I95CAP = VABCAD
038900040506     C                   EVAL      I95LOC = VABLOD
039000050627     C                   EVAL      I95NAR = VABNZD
039100040506     C                   CALL      'TISI95R'
039200040506     C                   PARM                    TISI95DS
039300040506     C                   EVAL      VABPRD = O95PRV
039400040506     C                   ENDIF
039500071121     C* CTR
039600071121     C                   EVAL      PiStr=%trim(%subst(vindta:172:3))
039700071121     C                   EXSR      CHKNUM
039800071121     C                   IF        PiInt=*on
039900071121     C                   Z-ADD     PiVal         VABCTR
040000071121     C                   ELSE
040100071121     C                   ADD       1             errore
040200071121     C                   EVAL      vinmsg = %trimr(vinmsg)
040300071121     C                             + ' ' + 'VABCTR'
040400071121     C                   ENDIF
040500060307     C* NCL
040600071121     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
040700071121     C                   EXSR      CHKNUM
040800071121     C                   IF        PiInt=*on
040900071121     C                   Z-ADD     PiVal         VABNCL
041000071121     C                   ELSE
041100071121     C                   ADD       1             errore
041200071121     C                   EVAL      vinmsg = %trimr(vinmsg)
041300071121     C                             + ' ' + 'VABNCL'
041400071121     C                   ENDIF
041500040506     C* PKB
041600071123     C                   EVAL      PiStr=%trim(%subst(vindta:196:8))
041700010201     C                   EXSR      CHKNUM
041800010201     C                   IF        PiNum=*on
041900071121     C                   Z-ADD     PiVal         VABPKB
042000010201     C                   ELSE
042100010201     C                   ADD       1             errore
042200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042300010201     C                             + ' ' + 'VABPKB'
042400010201     C                   ENDIF
042500071121     C* RMN
042600071121     C                   EVAL      PiStr=%trim(%subst(vindta:204:15))
042700071121     C                   EXSR      CHKNUM
042800071121     C                   IF        PiInt=*on
042900071121     C                   Z-ADD     PiVal         VABRMN
043000071121     C                   ELSE
043100071121     C                   ADD       1             errore
043200071121     C                   EVAL      vinmsg = %trimr(vinmsg)
043300071121     C                             + ' ' + 'VABRMN'
043400071121     C                   ENDIF
043500071121     C* NSP
043600071121     C                   EVAL      PiStr=%trim(%subst(vindta:11:7))
043700071019     C                   EXSR      CHKNUM
043800071019     C                   IF        PiInt=*on
043900071019     C                   Z-ADD     PiVal         VABNSP
044000071019     C                   Z-ADD     PiVal         VATNSP
044100071019     C                   ELSE
044200071019     C                   ADD       1             errore
044300071019     C                   EVAL      vinmsg = %trimr(vinmsg)
044400071121     C                             + ' ' + 'VABNSP VATNSP'
044500071019     C                   ENDIF
044600071121     C* LNP
044700071121     C                   EVAL      PiStr=%trim(%subst(vindta:8:3))
044800071121     C                   EXSR      CHKNUM
044900071121     C                   IF        PiInt=*on
045000071121     C                   Z-ADD     PiVal         VABLNP
045100071121     C                   Z-ADD     PiVal         VATLNP
045200081104     C                   IF        vlrpoi = 999
045300081104     C                   Z-ADD     PiVal         VABFGS
045400081104     C                   Z-ADD     PiVal         VATFGS
045500081105     C                   Z-ADD     VABFGS        vlrpoi
045600081104     C                   ENDIF
045700071121     C                   ELSE
045800071121     C                   ADD       1             errore
045900071121     C                   EVAL      vinmsg = %trimr(vinmsg)
046000071121     C                             + ' ' + 'VABLNP VATLNP'
046100071121     C                   ENDIF
046200150219     C* CAS
046300150219     C                   IF        %subst(vindta:373:13)<> *blanks    AND
046400150219     C                             %subst(vindta:137:10)<> *zeros
046500150219     C                   EVAL      FlgCAS = '1'
046600150219     C                   EVAL      VABVCA = 'EUR'
046700150219     C                   EVAL      PiStr=%trim(%subst(vindta:373:13))
046800150219     C                   EXSR      CHKNUM
046900150219     C                   IF        PiNum=*on
047000150219     C                   Z-ADD(H)  PiVal         VABCAS
047100150219     C                   ELSE
047200150219     C                   ADD       1             errore
047300150219     C                   EVAL      vinmsg = %trimr(vinmsg)
047400150219     C                             + ' ' + 'VABCAS'
047500150219     C                   ENDIF
047600150219     C                   ELSE
047700150219     C                   EVAL      FlgCAS = '0'
047800150219     C                   ENDIF
047900090223     C* IAS
048000090223     C                   IF        %subst(vindta:355:14) <> *zeros  AND
048100090223     C                             %subst(vindta:355:14) <> *blanks AND
048200090223     C                             %subst(vindta:355:14) <> '0000000000,000'
048300090223     C                   EVAL      PiStr=%trim(%subst(vindta:355:14))
048400090223     C                   EVAL      VABVAS='EUR'
048500090223     C                   EXSR      CHKNUM
048600090223     C                   IF        PiNum=*on
048700090223     C                   Z-ADD     PiVal         VABIAS
048800090223     C                   ELSE
048900090223     C                   ADD       1             errore
049000090223     C                   EVAL      vinmsg = %trimr(vinmsg)
049100090223     C                             + ' ' + 'VABIAS'
049200090223     C                   ENDIF
049300090223     C                   ENDIF
049400081014     C*
049500081014     C* Considerazioni particolari x diversi traffici cliente
049600081014     C                   if        vabNCL = *zeros
049700081014     C                   eval      vabNCL = 1
049800081014     C                   setoff                                       60
049900081014     C                   else
050000081014     C                   seton                                        60
050100081014     C                   endif
050200081014     C*
050300081014     C* Carico l'estensione A del FNVAT
050400081014     C                   exsr      exevata
050500071121     C*
050600071121     C* Carico l'estensione B del FNVAT
050700071121     C                   exsr      exevatb
050800010205     C*
050900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051000040802     C                   IF        FlgCAS <> '0'
051100070102     C                   IF        VABCBO = '1'
051200010205     C                   EVAL      VABCBO = '4'
051300150219     C                   ENDIF
051400150219     C                   IF        VABCBO = '2'
051500070102     C                   EVAL      VABCBO = '6'
051600070102     C                   ENDIF
051700010205     C                   ENDIF
051800020305     C*
051900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052000011113     C                   EXSR      CHKIMPDIV
052100010202     C*
052200000801     C* Ebbene...
052300000801     C                   ADD       1             §CTRMO
052400010201     C                   IF        errore <> *zeros
052500000801     C                   ADD       1             §CTRNO
052600000801     C                   EVAL      recko = vindta
052700000801     C                   EXCEPT    rigadett
052800000801     C                   EVAL      vinflg = '2'
052900000801     C                   ELSE
053000010201     C                   ADD       1             §CTROKVB
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400070102     C*----------------------------------------------------*
053500070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
053600070102     C*----------------------------------------------------*
053700070103     C     EXEVATE       BEGSR
053800070102     C*
053900070103     C                   EVAL      VATTRC='E'
054000081014     C*
054100081014     C                   if        *in60 = *off
054200081014     C                   EVAL      VATNOT = %trim(%subst(vindta:219:15))
054300081014     C                   exsr      wrivat                                       => scarico VAT
054400081014     C                   else
054500081014     C*
054600081014     C* X ogni collo indicato in testata
054700081014     C                   z-add     1             wColli            5 0
054800081014     C                   dow       wColli <= VABNCL
054900081014     C*
055000081014     C* Valorizzo il buffer di scrittura del FIVAT
055100081014     C                   move(p)   wColli        wColliAlfa        2
055200081014     C                   eval      VATNOT = %trim(%subst(vindta:219:15))+'C'+
055300081014     C                                      wColliAlfa
055400081014     C                   exsr      wrivat                                       => scarico VAT
055500081014     C*
055600081014     C                   add       1             wColli
055700081014     C                   enddo
055800081014     C*
055900081014     C                   endif
056000070102     C*
056100070102     C                   ENDSR
056200071121     C*----------------------------------------------------*
056300081014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
056400071121     C*----------------------------------------------------*
056500081014     C     EXEVATA       BEGSR
056600071121     C*
056700081014     C                   EVAL      VATTRC='A'
056800071121     C                   EVAL      VATNOT = %trim(%subst(vindta:269:35))
056900071121     C* Se nn passato imposto x default il '.'
057000071121     C                   IF        VATNOT = *blanks
057100071121     C                   EVAL      VATNOT = '.'
057200071121     C                   ENDIF
057300071121     C*
057400071121     C                   exsr      wrivat                                       => scarico VAT
057500071121     C*
057600071121     C                   ENDSR
057700081014     C*----------------------------------------------------*
057800081014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
057900081014     C*----------------------------------------------------*
058000081014     C     EXEVATB       BEGSR
058100081014     C*
058200081014     C                   EVAL      VATTRC='B'
058300081014     C                   EVAL      VATNOT = %trim(%subst(vindta:304:16))
058400081014     C* Se nn passato imposto x default il '.'
058500081014     C                   IF        VATNOT = *blanks
058600081014     C                   EVAL      VATNOT = '.'
058700081014     C                   ENDIF
058800081014     C*
058900081014     C                   exsr      wrivat                                       => scarico VAT
059000081014     C*
059100081014     C                   ENDSR
059200010201     C*----------------------------------------------------*
059300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
059400010201     C*----------------------------------------------------*
059500020305     C     WRIVAT        BEGSR
059600050628     C*
059700060223     C* Scrivo solo se valorizzato qualcosa
059800060223     C                   IF        VATNOT <> *blanks
059900040802     C                   WRITE     FIVAT000
060000060223     C                   ENDIF
060100010201     C*
060200010201     C                   ENDSR
060300010202     C*----------------------------------------------------*
060400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060500010202     C*----------------------------------------------------*
060600020305     C     PREVAT        BEGSR
060700010202     C*
060800021113     C* Compongo il nome del membro da dare al FIVATWWR
060900010202     C                   eval      parmbr = vlrhdl
061000010202     C                   movel     'M'           parmbr
061100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
061200010202     C                   eval      paropz = '1'
061300010202     C* Effettuo la chiamata al CLLE preposto
061400040506     C                   call(e)   'TITVVTC'
061500010202     C                   parm                    parccm
061600010202     C                   parm                    parmbr
061700010202     C                   parm                    paropz
061800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061900010202     C                   if        %error
062000010202     C                   movel     '1'           chkcall
062100010202     C                   else
062200010202     C                   movel     '0'           chkcall
062300010202     C                   endif
062400010202     C*
062500010202     C                   ENDSR
062600000801     C*----------------------------------------------------*
062700000801     C*  CONTROLLO NUMERICITA' CAMPI
062800000801     C*----------------------------------------------------*
062900000801     C     CHKNUM        BEGSR
063000000801     C*
063100000801     C                   call(e)   'ISNUMERIC'
063200000801     C                   PARM                    PiStr            30
063300070213     C                   PARM      ','           PiDecChr          1
063400000801     C                   PARM      *ZEROS        PiVal            30 9
063500000801     C                   PARM      '0'           PiInt             1
063600000801     C                   PARM      '0'           PiNum             1
063700000801     C                   IF        %error
063800000801     C                   EVAL      PiInt=*off
063900000801     C                   ENDIF
064000000801     C*
064100000801     C                   ENDSR
064200000801     C***
064300000801
064400011113
064500011113     C*----------------------------------------------------*
064600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064700011113     C*----------------------------------------------------*
064800011113     C     CHKIMPDIV     BEGSR
064900011113     C*
065000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065100011113     C                   Z-ADD     *zeros        wrkDec            9 9
065200011113     C*
065300011113     C* Come prima cosa effettuo considerazioni sulla divisa
065400011113     C                   IF        vabIAS > *zeros
065500011113     C                   IF        vabVAS <> 'EUR'
065600011113     C                   EVAL      vabVAS =  'ITL'
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C                   IF        vabCAS > *zeros
066100011113     C                   IF        vabVCA <> 'EUR'
066200011113     C                   EVAL      vabVCA =  'ITL'
066300011113     C                   ENDIF
066400011113     C                   ENDIF
066500011113     C*
066600011113     C                   IF        vabVMD > *zeros
066700020305     C                   IF        vabVAD <> 'EUR'
066800011113     C                   EVAL      vabVAD =  'ITL'
066900011113     C                   ENDIF
067000011113     C                   ENDIF
067100011113     C*
067200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067300011113     C                   Z-ADD     vabIAS        wrkDec
067400011113     C                   IF        wrkDec > *zeros
067500011113     C                   IF        vabVAS = 'ITL'
067600011113     C                   EVAL      vabIAS = *zeros
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068100011113     C                   Z-ADD     vabCAS        wrkDec
068200011113     C                   IF        wrkDec > *zeros
068300011113     C                   IF        vabVCA = 'ITL'
068400011113     C                   EVAL      vabCAS = *zeros
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068900011113     C                   Z-ADD     vabVMD        wrkDec
069000011113     C                   IF        wrkDec > *zeros
069100011113     C                   IF        vabVAD = 'ITL'
069200011113     C                   EVAL      vabVMD = *zeros
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C                   ENDSR
069700011113     C***
069800011113
069900011113
070000000801
070100000801
070200990920      /TITLE Invio dei dati al punto operativo.
070300010202     C     invio         BEGSR
070400990920     C*
070500021113     C* 1° invio FIVAT
070600010201     C                   reset                   dscmz
070700010201     C                   move      vlrpoi        cmzdst
070800021113     C                   eval      cmzfld = 'FIVATWWR'
070900010201     C                   eval      cmzmbd = vlrhdl
071000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071100021009     C***                if        prmfir = *blanks
071200021113     C                   eval      cmzfla = 'FIVAT00F'
071300021113     C                   eval      cmzmba = 'FIVAT00F'
071400021009     C***                else
071500021009     C***                eval      cmzfla = prmfir
071600021009     C***                eval      cmzmba = prmfir
071700021009     C***                endif
071800010201     C                   eval      cmznrr = *zeros
071900020305     C                   move      §ctrokvt      cmznrr
072000021018     C                   eval      cmzlba = vlrfl1
072100010201     C                   call(e)   'TIS711C'
072200010201     C                   parm                    dscmz
072300010201     C                   parm      *blanks       esito
072400010205     C                   if        %error
072500010205     C                             or cmzerr = '1'
072600010205     C                             or esito  = '1'
072700010205     C                   eval      wrkesito = '3'
072800010205     C                   else
072900010201     C*
073000021113     C* 2° invio FIVAB
073100010201     C                   reset                   dscmz
073200010201     C                   move      vlrpoi        cmzdst
073300010201     C                   eval      cmzfld = vlrfou
073400010201     C                   eval      cmzmbd = vlrhdl
073500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073600021009     C***                if        prmfir = *blanks
073700021113     C                   eval      cmzfla = 'FIVAB00F'
073800021113     C                   eval      cmzmba = 'FIVAB00F'
073900021009     C***                else
074000021009     C***                eval      cmzfla = prmfir
074100021009     C***                eval      cmzmba = prmfir
074200021009     C***                endif
074300010201     C                   eval      cmznrr = *zeros
074400010201     C                   move      §ctrokvb      cmznrr
074500021018     C                   eval      cmzlba = vlrfl1
074600010201     C                   call(e)   'TIS711C'
074700010201     C                   parm                    dscmz
074800010201     C                   parm      *blanks       esito
074900010201     C                   if        %error
075000010201     C                             or cmzerr = '1'
075100010201     C                             or esito  = '1'
075200010201     C                   eval      wrkesito = '3'
075300010201     C                   endif
075400010205     C                   endif
075500990920     C*
075600000613     C                   ENDSR
075700000613     C***
075800070411
075900070411     C     *pssr         BEGSR
076000070411     C*
076100070411     C                   if        %open(tivin00r)
076200070411     C                   close     tivin00r
076300070411     C                   endif
076400070411     C                   if        %open(fivabwwr)
076500070411     C                   close     fivabwwr
076600070411     C                   endif
076700070411     C                   if        %open(fivatwwr)
076800070411     C                   close     fivatwwr
076900070411     C                   endif
077000070411     C*
077100070411     C* Effettuo la chiamata al CLLE preposto
077200070411     C                   call(e)   'TITVVTC'
077300070411     C                   parm                    parccm
077400070411     C                   parm                    parmbr
077500070411     C                   parm      '2'           paropz
077600070411     C*
077700070411     C                   eval      wrkesito = '2'
077800070411     C*
077900070411     C                   seton                                        LR
078000070411     C*
078100070411     C                   ENDSR     '*CANCL'
078200070411     C***
078300070411
078400990910
078500000613     C     *inzsr        BEGSR
078600990910     C*
078700990910     C     *entry        plist
078800990920     C                   parm                    tivlrds
078900990921     C                   parm      wrkesito      esito
079000000724     C                   parm                    prmlit
079100000710     C                   parm                    prmfir
079200000613     C*
079300000830     C* CALCOLA LA DATA CORRENTE
079400000830     C                   time                    wn14             14 0
079500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
079600000830     C                   z-add     wn8           g08dat
079700000830     C                   z-add     *zeros        g08inv
079800000830     C                   movel     '0'           g08err
079900000830     C                   call      'XSRDA8'
080000000830     C                   parm                    wlbda8
080100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080200000830     C*
080300000613     C                   ENDSR
080400000613     C***
080500990908
080600071121     Otitv1c4p  E            riepilogo         2
080700990915     O                                              'Upload via Internet'
080800990915     O                                           +1 'Traduzione TIVIN00R -'
080900021113     O                                           +1 'FIVABWWR/FIVATWWR'
081000010201     O                                           +1 'Report testate bolle'
081100990915     O          E            riepilogo   2
081200990915     O                       wrkdata
081300990915     O                       wrkora              +1
081400990915     O                       procname            +1
081500990915     O          E            riepilogo   2
081600990915     O                                              'Cliente..................:'
081700990915     O                       VABCCM        z     +1
081800990915     O          E            riepilogo   2
081900990920     O                                              'Riferimento Strategi.....:'
082000990920     O                       vlrhdl              +1
082100990915     O          E            riepilogo   2
082200990915     O                                              'Giusti...................:'
082300010201     O                       §CTROKVB      2   +  1
082400990915     O          E            riepilogo   2
082500990915     O                                              'Sbagliati e corretti.....:'
082600971022     O                       §CTRMO        2   +  1
082700990915     O          E            riepilogo   2
082800990915     O                                              'Sbagliati e scartati.....:'
082900971022     O                       §CTRNO        2   +  1
083000000613
083100071121     Otitv1c4ps E            testdett          2
083200000613     O                                              'Upload via Internet'
083300000613     O                                           +1 'Traduzione TIVIN00R -'
083400021113     O                                           +1 'FIVABWWR/FIVATWWR'
083500010201     O                                           +1 'Report testate bolle'
083600000616     O          E            testdett    3
083700000613     O                                           +2 'N° rec'
083800000613     O                                           +3 'Anteprima contenuto'
083900000616     O          E            rigadett    2
084000000613     O                       rrnum               +2
084100000621     O                       recko               +3
084200000616     O          E            findett     2
084300000613     O                       wrkdata
084400000613     O                       wrkora              +1
084500000613     O                       procname            +1
084600000616     O          E            findett     2
084700000613     O                                              'Cliente..................:'
084800000613     O                       VABCCM        z     +1
084900000616     O          E            findett     2
085000000613     O                                              'Riferimento Strategi.....:'
085100000613     O                       vlrhdl              +1
085200000616     O          E            findett     2
085300000613     O                                              'Giusti...................:'
085400010201     O                       §CTROKVB      2   +  1
085500000616     O          E            findett     2
085600000613     O                                              'Sbagliati e corretti.....:'
085700000613     O                       §CTRMO        2   +  1
085800000616     O          E            findett     2
085900000613     O                                              'Sbagliati e scartati.....:'
086000000613     O                       §CTRNO        2   +  1
086100000512** CMD - COMANDI CL
086200071121OVRPRTF FILE(TITV1C4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
086300071121OVRPRTF FILE(TITV1C4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
086400071121DLTOVR FILE(TITV1C4P TITV1C4PS) LVL(*)
086500000512
086600000512
