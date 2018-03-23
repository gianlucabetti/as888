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
034800090716     C* CCM
034900090716     C                   IF        %subst(vindta:1:7) <> *blanks AND
035000090716     C                             %subst(vindta:1:7) <> *zeros
035100090716     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
035200090716     C                   EXSR      CHKNUM
035300090716     C                   IF        PiInt=*on
035400090716     C                   Z-ADD     PiVal         VABCCM
035500090909     C                   Z-ADD     PiVal         VATCCM
035600090716     C                   ELSE
035700090716     C                   ADD       1             errore
035800090716     C                   EVAL      vinmsg = %trimr(vinmsg)
035900090909     C                             + ' ' + 'VABCCM VATCCM'
036000090716     C                   ENDIF
036100090716     C                   ENDIF
036200090716     C*
036300090716     C                   IF        %subst(vindta:1:7) = '0054780'
036400090716     C                   EVAL      VABCCM=0054780
036500090716     C                   EVAL      VATCCM=0054780
036600090716     C                   EVAL      VABTC1='A'
036700090716     C                   ENDIF
036800050628     C* CAP
036900071121     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
037000010201     C                   EXSR      CHKNUM
037100010201     C                   IF        PiInt=*on
037200010201     C                   Z-ADD     PiVal         Num5_0
037300040506     C                   MOVEL(P)  Num5_0        VABCAD
037400010201     C                   ELSE
037500040506     C                   ADD       1             errore
037600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037700040506     C                             + ' ' + 'VABCAD'
037800010201     C                   ENDIF
037900040506     C* Reperisco la provincia dal CAP e dalla località
038000040526     C                   IF        VABCAD <> *blanks AND
038100040526     C                             VABPRD  = *blanks
038200040506     C                   CLEAR                   TISI95DS
038300040506     C                   EVAL      I95TCN = '3'
038400040506     C                   Z-ADD     datcor        I95DAT
038500040506     C                   EVAL      I95CAP = VABCAD
038600040506     C                   EVAL      I95LOC = VABLOD
038700050627     C                   EVAL      I95NAR = VABNZD
038800040506     C                   CALL      'TISI95R'
038900040506     C                   PARM                    TISI95DS
039000040506     C                   EVAL      VABPRD = O95PRV
039100040506     C                   ENDIF
039200071121     C* CTR
039300071121     C                   EVAL      PiStr=%trim(%subst(vindta:172:3))
039400071121     C                   EXSR      CHKNUM
039500071121     C                   IF        PiInt=*on
039600071121     C                   Z-ADD     PiVal         VABCTR
039700071121     C                   ELSE
039800071121     C                   ADD       1             errore
039900071121     C                   EVAL      vinmsg = %trimr(vinmsg)
040000071121     C                             + ' ' + 'VABCTR'
040100071121     C                   ENDIF
040200060307     C* NCL
040300071121     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
040400071121     C                   EXSR      CHKNUM
040500071121     C                   IF        PiInt=*on
040600071121     C                   Z-ADD     PiVal         VABNCL
040700071121     C                   ELSE
040800071121     C                   ADD       1             errore
040900071121     C                   EVAL      vinmsg = %trimr(vinmsg)
041000071121     C                             + ' ' + 'VABNCL'
041100071121     C                   ENDIF
041200040506     C* PKB
041300071123     C                   EVAL      PiStr=%trim(%subst(vindta:196:8))
041400010201     C                   EXSR      CHKNUM
041500010201     C                   IF        PiNum=*on
041600071121     C                   Z-ADD     PiVal         VABPKB
041700010201     C                   ELSE
041800010201     C                   ADD       1             errore
041900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042000010201     C                             + ' ' + 'VABPKB'
042100010201     C                   ENDIF
042200071121     C* RMN
042300071121     C                   EVAL      PiStr=%trim(%subst(vindta:204:15))
042400071121     C                   EXSR      CHKNUM
042500071121     C                   IF        PiInt=*on
042600071121     C                   Z-ADD     PiVal         VABRMN
042700071121     C                   ELSE
042800071121     C                   ADD       1             errore
042900071121     C                   EVAL      vinmsg = %trimr(vinmsg)
043000071121     C                             + ' ' + 'VABRMN'
043100071121     C                   ENDIF
043200071121     C* NSP
043300071121     C                   EVAL      PiStr=%trim(%subst(vindta:11:7))
043400071019     C                   EXSR      CHKNUM
043500071019     C                   IF        PiInt=*on
043600071019     C                   Z-ADD     PiVal         VABNSP
043700071019     C                   Z-ADD     PiVal         VATNSP
043800071019     C                   ELSE
043900071019     C                   ADD       1             errore
044000071019     C                   EVAL      vinmsg = %trimr(vinmsg)
044100071121     C                             + ' ' + 'VABNSP VATNSP'
044200071019     C                   ENDIF
044300071121     C* LNP
044400071121     C                   EVAL      PiStr=%trim(%subst(vindta:8:3))
044500071121     C                   EXSR      CHKNUM
044600071121     C                   IF        PiInt=*on
044700071121     C                   Z-ADD     PiVal         VABLNP
044800071121     C                   Z-ADD     PiVal         VATLNP
044900081104     C                   IF        vlrpoi = 999
045000081104     C                   Z-ADD     PiVal         VABFGS
045100081104     C                   Z-ADD     PiVal         VATFGS
045200081105     C                   Z-ADD     VABFGS        vlrpoi
045300081104     C                   ENDIF
045400071121     C                   ELSE
045500071121     C                   ADD       1             errore
045600071121     C                   EVAL      vinmsg = %trimr(vinmsg)
045700071121     C                             + ' ' + 'VABLNP VATLNP'
045800071121     C                   ENDIF
045900090223     C* IAS
046000090223     C                   IF        %subst(vindta:355:14) <> *zeros  AND
046100090223     C                             %subst(vindta:355:14) <> *blanks AND
046200090223     C                             %subst(vindta:355:14) <> '0000000000,000'
046300090223     C                   EVAL      PiStr=%trim(%subst(vindta:355:14))
046400090223     C                   EVAL      VABVAS='EUR'
046500090223     C                   EXSR      CHKNUM
046600090223     C                   IF        PiNum=*on
046700090223     C                   Z-ADD     PiVal         VABIAS
046800090223     C                   ELSE
046900090223     C                   ADD       1             errore
047000090223     C                   EVAL      vinmsg = %trimr(vinmsg)
047100090223     C                             + ' ' + 'VABIAS'
047200090223     C                   ENDIF
047300090223     C                   ENDIF
047400081014     C*
047500081014     C* Considerazioni particolari x diversi traffici cliente
047600081014     C                   if        vabNCL = *zeros
047700081014     C                   eval      vabNCL = 1
047800081014     C                   setoff                                       60
047900081014     C                   else
048000081014     C                   seton                                        60
048100081014     C                   endif
048200081014     C*
048300081014     C* Carico l'estensione A del FNVAT
048400081014     C                   exsr      exevata
048500071121     C*
048600071121     C* Carico l'estensione B del FNVAT
048700071121     C                   exsr      exevatb
048800010205     C*
048900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
049000040802     C                   IF        FlgCAS <> '0'
049100070102     C                   IF        VABCBO = '1'
049200010205     C                   EVAL      VABCBO = '4'
049300010205     C                   ELSE
049400070102     C                   EVAL      VABCBO = '6'
049500070102     C                   ENDIF
049600010205     C                   ENDIF
049700020305     C*
049800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049900011113     C                   EXSR      CHKIMPDIV
050000010202     C*
050100000801     C* Ebbene...
050200000801     C                   ADD       1             §CTRMO
050300010201     C                   IF        errore <> *zeros
050400000801     C                   ADD       1             §CTRNO
050500000801     C                   EVAL      recko = vindta
050600000801     C                   EXCEPT    rigadett
050700000801     C                   EVAL      vinflg = '2'
050800000801     C                   ELSE
050900010201     C                   ADD       1             §CTROKVB
051000000801     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300070102     C*----------------------------------------------------*
051400070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
051500070102     C*----------------------------------------------------*
051600070103     C     EXEVATE       BEGSR
051700070102     C*
051800070103     C                   EVAL      VATTRC='E'
051900081014     C*
052000081014     C                   if        *in60 = *off
052100081014     C                   EVAL      VATNOT = %trim(%subst(vindta:219:15))
052200081014     C                   exsr      wrivat                                       => scarico VAT
052300081014     C                   else
052400081014     C*
052500081014     C* X ogni collo indicato in testata
052600081014     C                   z-add     1             wColli            5 0
052700081014     C                   dow       wColli <= VABNCL
052800081014     C*
052900081014     C* Valorizzo il buffer di scrittura del FIVAT
053000081014     C                   move(p)   wColli        wColliAlfa        2
053100081014     C                   eval      VATNOT = %trim(%subst(vindta:219:15))+'C'+
053200081014     C                                      wColliAlfa
053300081014     C                   exsr      wrivat                                       => scarico VAT
053400081014     C*
053500081014     C                   add       1             wColli
053600081014     C                   enddo
053700081014     C*
053800081014     C                   endif
053900070102     C*
054000070102     C                   ENDSR
054100071121     C*----------------------------------------------------*
054200081014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
054300071121     C*----------------------------------------------------*
054400081014     C     EXEVATA       BEGSR
054500071121     C*
054600081014     C                   EVAL      VATTRC='A'
054700071121     C                   EVAL      VATNOT = %trim(%subst(vindta:269:35))
054800071121     C* Se nn passato imposto x default il '.'
054900071121     C                   IF        VATNOT = *blanks
055000071121     C                   EVAL      VATNOT = '.'
055100071121     C                   ENDIF
055200071121     C*
055300071121     C                   exsr      wrivat                                       => scarico VAT
055400071121     C*
055500071121     C                   ENDSR
055600081014     C*----------------------------------------------------*
055700081014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
055800081014     C*----------------------------------------------------*
055900081014     C     EXEVATB       BEGSR
056000081014     C*
056100081014     C                   EVAL      VATTRC='B'
056200081014     C                   EVAL      VATNOT = %trim(%subst(vindta:304:16))
056300081014     C* Se nn passato imposto x default il '.'
056400081014     C                   IF        VATNOT = *blanks
056500081014     C                   EVAL      VATNOT = '.'
056600081014     C                   ENDIF
056700081014     C*
056800081014     C                   exsr      wrivat                                       => scarico VAT
056900081014     C*
057000081014     C                   ENDSR
057100010201     C*----------------------------------------------------*
057200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
057300010201     C*----------------------------------------------------*
057400020305     C     WRIVAT        BEGSR
057500050628     C*
057600060223     C* Scrivo solo se valorizzato qualcosa
057700060223     C                   IF        VATNOT <> *blanks
057800040802     C                   WRITE     FIVAT000
057900060223     C                   ENDIF
058000010201     C*
058100010201     C                   ENDSR
058200010202     C*----------------------------------------------------*
058300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058400010202     C*----------------------------------------------------*
058500020305     C     PREVAT        BEGSR
058600010202     C*
058700021113     C* Compongo il nome del membro da dare al FIVATWWR
058800010202     C                   eval      parmbr = vlrhdl
058900010202     C                   movel     'M'           parmbr
059000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
059100010202     C                   eval      paropz = '1'
059200010202     C* Effettuo la chiamata al CLLE preposto
059300040506     C                   call(e)   'TITVVTC'
059400010202     C                   parm                    parccm
059500010202     C                   parm                    parmbr
059600010202     C                   parm                    paropz
059700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059800010202     C                   if        %error
059900010202     C                   movel     '1'           chkcall
060000010202     C                   else
060100010202     C                   movel     '0'           chkcall
060200010202     C                   endif
060300010202     C*
060400010202     C                   ENDSR
060500000801     C*----------------------------------------------------*
060600000801     C*  CONTROLLO NUMERICITA' CAMPI
060700000801     C*----------------------------------------------------*
060800000801     C     CHKNUM        BEGSR
060900000801     C*
061000000801     C                   call(e)   'ISNUMERIC'
061100000801     C                   PARM                    PiStr            30
061200070213     C                   PARM      ','           PiDecChr          1
061300000801     C                   PARM      *ZEROS        PiVal            30 9
061400000801     C                   PARM      '0'           PiInt             1
061500000801     C                   PARM      '0'           PiNum             1
061600000801     C                   IF        %error
061700000801     C                   EVAL      PiInt=*off
061800000801     C                   ENDIF
061900000801     C*
062000000801     C                   ENDSR
062100000801     C***
062200000801
062300011113
062400011113     C*----------------------------------------------------*
062500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062600011113     C*----------------------------------------------------*
062700011113     C     CHKIMPDIV     BEGSR
062800011113     C*
062900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063000011113     C                   Z-ADD     *zeros        wrkDec            9 9
063100011113     C*
063200011113     C* Come prima cosa effettuo considerazioni sulla divisa
063300011113     C                   IF        vabIAS > *zeros
063400011113     C                   IF        vabVAS <> 'EUR'
063500011113     C                   EVAL      vabVAS =  'ITL'
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C                   IF        vabCAS > *zeros
064000011113     C                   IF        vabVCA <> 'EUR'
064100011113     C                   EVAL      vabVCA =  'ITL'
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   IF        vabVMD > *zeros
064600020305     C                   IF        vabVAD <> 'EUR'
064700011113     C                   EVAL      vabVAD =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065200011113     C                   Z-ADD     vabIAS        wrkDec
065300011113     C                   IF        wrkDec > *zeros
065400011113     C                   IF        vabVAS = 'ITL'
065500011113     C                   EVAL      vabIAS = *zeros
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066000011113     C                   Z-ADD     vabCAS        wrkDec
066100011113     C                   IF        wrkDec > *zeros
066200011113     C                   IF        vabVCA = 'ITL'
066300011113     C                   EVAL      vabCAS = *zeros
066400011113     C                   ENDIF
066500011113     C                   ENDIF
066600011113     C*
066700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066800011113     C                   Z-ADD     vabVMD        wrkDec
066900011113     C                   IF        wrkDec > *zeros
067000011113     C                   IF        vabVAD = 'ITL'
067100011113     C                   EVAL      vabVMD = *zeros
067200011113     C                   ENDIF
067300011113     C                   ENDIF
067400011113     C*
067500011113     C                   ENDSR
067600011113     C***
067700011113
067800011113
067900000801
068000000801
068100990920      /TITLE Invio dei dati al punto operativo.
068200010202     C     invio         BEGSR
068300990920     C*
068400021113     C* 1° invio FIVAT
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700021113     C                   eval      cmzfld = 'FIVATWWR'
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAT00F'
069200021113     C                   eval      cmzmba = 'FIVAT00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800020305     C                   move      §ctrokvt      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010205     C                   if        %error
070400010205     C                             or cmzerr = '1'
070500010205     C                             or esito  = '1'
070600010205     C                   eval      wrkesito = '3'
070700010205     C                   else
070800010201     C*
070900021113     C* 2° invio FIVAB
071000010201     C                   reset                   dscmz
071100010201     C                   move      vlrpoi        cmzdst
071200010201     C                   eval      cmzfld = vlrfou
071300010201     C                   eval      cmzmbd = vlrhdl
071400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500021009     C***                if        prmfir = *blanks
071600021113     C                   eval      cmzfla = 'FIVAB00F'
071700021113     C                   eval      cmzmba = 'FIVAB00F'
071800021009     C***                else
071900021009     C***                eval      cmzfla = prmfir
072000021009     C***                eval      cmzmba = prmfir
072100021009     C***                endif
072200010201     C                   eval      cmznrr = *zeros
072300010201     C                   move      §ctrokvb      cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500010201     C                   call(e)   'TIS711C'
072600010201     C                   parm                    dscmz
072700010201     C                   parm      *blanks       esito
072800010201     C                   if        %error
072900010201     C                             or cmzerr = '1'
073000010201     C                             or esito  = '1'
073100010201     C                   eval      wrkesito = '3'
073200010201     C                   endif
073300010205     C                   endif
073400990920     C*
073500000613     C                   ENDSR
073600000613     C***
073700070411
073800070411     C     *pssr         BEGSR
073900070411     C*
074000070411     C                   if        %open(tivin00r)
074100070411     C                   close     tivin00r
074200070411     C                   endif
074300070411     C                   if        %open(fivabwwr)
074400070411     C                   close     fivabwwr
074500070411     C                   endif
074600070411     C                   if        %open(fivatwwr)
074700070411     C                   close     fivatwwr
074800070411     C                   endif
074900070411     C*
075000070411     C* Effettuo la chiamata al CLLE preposto
075100070411     C                   call(e)   'TITVVTC'
075200070411     C                   parm                    parccm
075300070411     C                   parm                    parmbr
075400070411     C                   parm      '2'           paropz
075500070411     C*
075600070411     C                   eval      wrkesito = '2'
075700070411     C*
075800070411     C                   seton                                        LR
075900070411     C*
076000070411     C                   ENDSR     '*CANCL'
076100070411     C***
076200070411
076300990910
076400000613     C     *inzsr        BEGSR
076500990910     C*
076600990910     C     *entry        plist
076700990920     C                   parm                    tivlrds
076800990921     C                   parm      wrkesito      esito
076900000724     C                   parm                    prmlit
077000000710     C                   parm                    prmfir
077100000613     C*
077200000830     C* CALCOLA LA DATA CORRENTE
077300000830     C                   time                    wn14             14 0
077400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
077500000830     C                   z-add     wn8           g08dat
077600000830     C                   z-add     *zeros        g08inv
077700000830     C                   movel     '0'           g08err
077800000830     C                   call      'XSRDA8'
077900000830     C                   parm                    wlbda8
078000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
078100000830     C*
078200000613     C                   ENDSR
078300000613     C***
078400990908
078500071121     Otitv1c4p  E            riepilogo         2
078600990915     O                                              'Upload via Internet'
078700990915     O                                           +1 'Traduzione TIVIN00R -'
078800021113     O                                           +1 'FIVABWWR/FIVATWWR'
078900010201     O                                           +1 'Report testate bolle'
079000990915     O          E            riepilogo   2
079100990915     O                       wrkdata
079200990915     O                       wrkora              +1
079300990915     O                       procname            +1
079400990915     O          E            riepilogo   2
079500990915     O                                              'Cliente..................:'
079600990915     O                       VABCCM        z     +1
079700990915     O          E            riepilogo   2
079800990920     O                                              'Riferimento Strategi.....:'
079900990920     O                       vlrhdl              +1
080000990915     O          E            riepilogo   2
080100990915     O                                              'Giusti...................:'
080200010201     O                       §CTROKVB      2   +  1
080300990915     O          E            riepilogo   2
080400990915     O                                              'Sbagliati e corretti.....:'
080500971022     O                       §CTRMO        2   +  1
080600990915     O          E            riepilogo   2
080700990915     O                                              'Sbagliati e scartati.....:'
080800971022     O                       §CTRNO        2   +  1
080900000613
081000071121     Otitv1c4ps E            testdett          2
081100000613     O                                              'Upload via Internet'
081200000613     O                                           +1 'Traduzione TIVIN00R -'
081300021113     O                                           +1 'FIVABWWR/FIVATWWR'
081400010201     O                                           +1 'Report testate bolle'
081500000616     O          E            testdett    3
081600000613     O                                           +2 'N° rec'
081700000613     O                                           +3 'Anteprima contenuto'
081800000616     O          E            rigadett    2
081900000613     O                       rrnum               +2
082000000621     O                       recko               +3
082100000616     O          E            findett     2
082200000613     O                       wrkdata
082300000613     O                       wrkora              +1
082400000613     O                       procname            +1
082500000616     O          E            findett     2
082600000613     O                                              'Cliente..................:'
082700000613     O                       VABCCM        z     +1
082800000616     O          E            findett     2
082900000613     O                                              'Riferimento Strategi.....:'
083000000613     O                       vlrhdl              +1
083100000616     O          E            findett     2
083200000613     O                                              'Giusti...................:'
083300010201     O                       §CTROKVB      2   +  1
083400000616     O          E            findett     2
083500000613     O                                              'Sbagliati e corretti.....:'
083600000613     O                       §CTRMO        2   +  1
083700000616     O          E            findett     2
083800000613     O                                              'Sbagliati e scartati.....:'
083900000613     O                       §CTRNO        2   +  1
084000000512** CMD - COMANDI CL
084100071121OVRPRTF FILE(TITV1C4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
084200071121OVRPRTF FILE(TITV1C4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
084300071121DLTOVR FILE(TITV1C4P TITV1C4PS) LVL(*)
084400000512
084500000512
