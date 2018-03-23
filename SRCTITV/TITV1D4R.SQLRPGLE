000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080117     Ftitv1d4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080117     Ftitv1d4ps O    f  198        PRINTER usropn
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
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600070928     D depspe          s             16    INZ(*blanks)
003700070928     D curspe          s             16    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300070530     D Num5_0          s              5  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400020725
005500020725     D*------------------
005600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005700020725     D*------------------
005800070530     D INPUT_DS        DS                  INZ
005900020725     D  VINDTA                 1   2048
006000020725     D  VINFLG              2049   2049
006100020725     D  VINSPE              2050   2059
006200020725     D  VINRRN              2060   2067  0
006300020725     D*
006400990908
006500010201
006600010201
006700080117     C*
006800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006900080117     C
007000080117     C/EXEC SQL
007100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007200080117     C/END-EXEC
007300080117     C
007400990915     C                   time                    wrkdata
007500990915     C                   time                    wrkora
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000724     C*
008000000724     C* SE OCCORRE SPEDIRE IN FILIALE
008100000724     C                   if        vlrpoi <> *zeros
008200000724     C*
008300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000724     C     vlrpoi        chain     azorg01l
008600000724     C                   if        %found
008700000616     C                   movel(p)  CMD(1)        depcmd
008800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000724     C*
009600000724     C                   endif
009700000724     C                   endif
009800000616     C*
009900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000616     C                   movel(p)  CMD(2)        depcmd
010100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010200000616     C*
010300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010400011113     C                   Z-ADD     150           LENGH            15 5
010500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010600000616     C                   PARM                    depcmd
010700000616     C                   PARM                    LENGH
010800000616     C*
010900080117     C                   if        not %open(titv1d4ps)
011000080117     C                   open      titv1d4ps
011100000616     C                   except    testdett
011200000613     C                   endif
011300000613     C*
011400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
011500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600000613     C*
011700010202     C* Effettuo la chiamata al CLLE preposto
011800050128     C                   call(e)   'TITVVTC'
011900010202     C                   parm                    parccm
012000010202     C                   parm                    parmbr
012100010202     C                   parm      '2'           paropz
012200070730     C*
012300070730     C* Effettuo lancio TISI95 solo x chiusura
012400070730     C                   CLEAR                   TISI95DS
012500070730     C                   EVAL      I95TLA = 'C'
012600070730     C                   CALL      'TISI95R'
012700070730     C                   PARM                    TISI95DS
012800010202     C*
012900080117     C                   if        %open(titv1d4ps)
013000000616     C                   except    findett
013100080117     C                   close     titv1d4ps
013200000613     C                   endif
013300000616     C*
013400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013500011113     C                   Z-ADD     150           LENGH            15 5
013600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013700000616     C                   PARM                    CMD(3)
013800000616     C                   PARM                    LENGH
013900000616     C*
014000000801     C
014100010201     C                   seton                                        LR
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800080117     C                   if        not %open(titv1d4p)
014900080117     C                   open      titv1d4p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400080117     C                   if        %open(titv1d4p)
015500080117     C                   close     titv1d4p
015600990914     C                   endif
015700990910     C*
015800000613     C                   ENDSR
015900000613     C***
016000990908
016100000801
016200910830     C*--------------------------------------------------------
016300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016400910830     C*--------------------------------------------------------
016500070530     C     RWFILE        BEGSR
016600990910     C*
016700990914     C                   if        not %open(tivin00r)
016800990908     C                   open      tivin00r
016900990914     C                   endif
017000021113     C                   if        not %open(fivabwwr)
017100021113     C                   open      fivabwwr
017200990914     C                   endif
017300070530     C*
017400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017500020305     C                   exsr      prevat
017600010201     C*
017700010202     C                   if        chkcall = '0'
017800010202     C*
017900021113     C                   if        not %open(fivatwwr)
018000021113     C                   open      fivatwwr
018100010201     C                   endif
018200080117     C*
018300080117     C                   EXSR      INZVAR
018400080117     C                   EXSR      DEFCAM
018500990910     C*
018600010201     C                   clear                   §CTROKVB          5 0
018700020305     C                   clear                   §CTROKVT          5 0
018800000801     C                   clear                   §CTRMO            5 0
018900000801     C                   clear                   §CTRNO            5 0
019000990910     C*
019100020725     C/EXEC SQL
019200020725     C+ declare C1 cursor for select
019300070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
019400060223     C+ from tivin00r
019500060223     C+ order by sped
019600020725     C+ for read only
019700020725     C/END-EXEC
019800020725     C
019900020725     C/EXEC SQL
020000020725     C+ open C1
020100020725     C/END-EXEC
020200020725     C
020300020725     C/EXEC SQL
020400070530     C+ Fetch C1 into :INPUT_DS
020500020725     C/END-EXEC
020600020725     C*
020700020725     C                   dow       sqlcod = *zeros
020800990913     C*
020900020725     C                   if        vindta > *blanks
021000000613     C                   add       1             rrnum
021100000801     C*
021200020725     C                   if        vinflg = *blanks
021300020725     C                             or vinflg = '0'
021400020725     C                             or vinflg = '2'
021500000801     C*
021600020725     C                   clear                   x_vinmsg
021700020725     C                   eval      x_vinflg = '1'
021800010305     C*
021900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022000070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
022100020305     C                   MOVEL(p)  PiStr         curspe
022200010305     C*
022300071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
022400071003     C                   if        *in50 = *off
022500071003     C                   exsr      impvab
022600071003     C                   exsr      wrivab
022700071003     C                   exsr      wrivat_b                                     => carico VAT
022800071003     C                   else
022900071003     C*
023000071009     C                   if        wDISK = 'D'
023100071009     C                   exsr      impvab
023200071009     C                   exsr      wrivab
023300071009     C                   exsr      wrivat_b                                     => carico VAT
023400071009     C                   exsr      wrivat_e                                     => carico VAT
023500071009     C                   else
023600071009     C*
023700010305     C                   if        depspe = *blanks                             => 1° giro
023800010305     C                   eval      depspe = curspe                              => memorizz. spediz
023900080117     C                   clear                   tivinds
024000020305     C                   exsr      impvab
024100071003     C                   exsr      wrivat_b                                     => carico VAT
024200071003     C   50              exsr      wrivat_e                                     => carico VAT
024300010305     C                   else
024400020725     C                   if        depspe <> curspe                             => rottura di spediz
024500010305     C                   eval      depspe = curspe                              => memorizz. spediz
024600070928     C                   exsr      wrivab
024700080117     C                   clear                   tivinds
024800020305     C                   exsr      impvab
024900071003     C                   exsr      wrivat_b                                     => carico VAT
025000071003     C   50              exsr      wrivat_e                                     => carico VAT
025100020305     C                   else                                                   => x stessa spediz
025200020305     C                   exsr      impvab
025300071003     C                   exsr      wrivat_b                                     => carico VAT
025400071003     C   50              exsr      wrivat_e                                     => carico VAT
025500010305     C                   endif
025600010305     C                   endif
025700010305     C                   endif
025800071003     C                   endif
025900071009     C                   endif
026000000905     C*
026100000905     C                   else
026200020725     C                   eval      x_vinflg = '1'
026300000905     C                   endif
026400000905     C*
026500020725     C     VINRRN        chain     tivin000
026600020725     C                   if        %found(tivin00r)
026700020725     C                   eval      y_vinflg = x_vinflg
026800020725     C                   eval      y_vinmsg = x_vinmsg
026900020725     C                   update    tivin000
027000020725     C                   endif
027100020725     C*
027200020725     C/EXEC SQL
027300070530     C+ Fetch C1 into :INPUT_DS
027400020725     C/END-EXEC
027500020725     C*
027600020725     C                   enddo
027700020725     C*
027800020725     C/EXEC SQL
027900020725     C+ close C1
028000020725     C/END-EXEC
028100000905     C*
028200020305     C* Scarico i VAB rimasti "in sospeso"
028300071009     C                   if        wDISK = 'C'
028400071009     C                   exsr      wrivab
028500071009     C                   endif
028600010202     C*
028700010202     C                   endif
028800990910
028900990910     C* Se non ci sono record con errori ...
029000000710     C                   if        §ctrno = 0
029100990910     C* ... restituisco esito OK.
029200990921     C                   eval      wrkesito = '0'
029300990910     C                   else
029400010201     C                   if        §ctrokvb > 0
029500990921     C                   eval      wrkesito = '1'
029600000710     C                   else
029700000710     C                   eval      wrkesito = '2'
029800990910     C                   endif
029900000710     C                   endif
030000990910     C*
030100990914     C                   if        %open(tivin00r)
030200990908     C                   close     tivin00r
030300990914     C                   endif
030400021113     C                   if        %open(fivabwwr)
030500021113     C                   close     fivabwwr
030600990914     C                   endif
030700021113     C                   if        %open(fivatwwr)
030800021113     C                   close     fivatwwr
030900010201     C                   endif
031000990910     C*
031100010201     C                   if        §ctrokvb > 0
031200000724     C                             and vlrpoi <> *zeros
031300010202     C                   exsr      invio
031400990920     C                   endif
031500990920     C*
031600910830     C                   ENDSR
031700000613     C***
031800010305
031900010305     C*----------------------------------------------------*
032000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
032100010305     C*----------------------------------------------------*
032200020305     C     WRIVAB        BEGSR
032300070730     C*
032400071003     C* Considerazioni finali
032500071003     C                   if        VABRMA = *blanks
032600071003     C                   movel(P)  VABRMN        VABRMA
032700071003     C                   endif
032800071003     C*
032900021113     C                   write     fivab000                                     => scarico il VAB
033000010305     C*
033100010305     C                   ENDSR
033200990920
033300000801     C*----------------------------------------------------*
033400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033500000801     C*----------------------------------------------------*
033600010201     C     INZVAR        BEGSR
033700000801     C*
033800010201     C                   Z-ADD     *zeros        Num5_0
033900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034000020725     C                   MOVEL     '0'           FlgCAS            1
034100000801     C*
034200000801     C                   ENDSR
034300000801     C*----------------------------------------------------*
034400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034500000801     C*----------------------------------------------------*
034600020904     C     DEFCAM        BEGSR
034700000801     C*
034800021113     C                   CLEAR                   FIVAB000
034900021113     C                   CLEAR                   FIVAT000
035000070730     C* Imposto i valori di default...
035100080117     C                   EVAL      VABCCM = 0410780
035200080117     C                   EVAL      VATCCM = 0410780
035300080117     C                   EVAL      VABLNP = 041
035400080117     C                   EVAL      VATLNP = 041
035500070928     C                   EVAL      VABCTR = 000
035600070730     C                   EVAL      VABCBO = '1'
035700070222     C* ... e poi verifico se sono stati passati come parametri
035800070222     C                   IF        vlrppt > *blanks
035900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036100070222     C                   EXSR      CHKNUM
036200070222     C                   IF        PiInt=*on
036300070222     C                   Z-ADD     PiVal         VABCCM
036400070222     C                   Z-ADD     PiVal         VATCCM
036500070222     C                   ENDIF
036600070222     C                   ENDIF
036700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036900070222     C                   EXSR      CHKNUM
037000070222     C                   IF        PiInt=*on
037100070222     C                   Z-ADD     PiVal         VABLNP
037200070222     C                   Z-ADD     PiVal         VATLNP
037300070222     C                   ENDIF
037400070222     C                   ENDIF
037500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037700070222     C                   EXSR      CHKNUM
037800070222     C                   IF        PiInt=*on
037900070222     C                   Z-ADD     PiVal         VABCTR
038000070222     C                   ENDIF
038100070928     C                   ENDIF
038200071009     C                   MOVEL     *blanks       wDISK             1
038300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038400071009     C                   IF        wDISK <> *blanks
038500070928     C                   SETON                                        50
038600070928     C                   ELSE
038700070928     C                   SETOFF                                       50
038800070222     C                   ENDIF
038900070222     C                   ENDIF
039000071009     C*
039100071009     C   50              EVAL      VABCTM = '7Q'
039200000801     C*
039300000801     C                   ENDSR
039400000801     C*----------------------------------------------------*
039500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039600000801     C*----------------------------------------------------*
039700010201     C     IMPVAB        BEGSR
039800070530     C*
039900070530     C                   SETOFF                                       3132
040000070928     C*
040100070928     C                   EXSR      INZVAR
040200070928     C                   EXSR      DEFCAM
040300070928     C*
040400070928     C                   MOVE(P)   vlrpoi        VABFGS
040500070928     C                   MOVE(P)   vlrpoi        VATFGS
040600070928     C*
040700070928     C                   MOVEL     datcor        VABAAS
040800070928     C                   MOVEL     datcor        VATAAS
040900070928     C                   MOVE      datcor        VABMGS
041000070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
041100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
041300070928     C* ==
041400070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
041500070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
041600070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
041700070928     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
041800070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
041900070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
042000070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
042100070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
042200070928     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
042300070928     C                   EVAL      VABTSP='E'
042400070928     C                   ENDIF
042500071003     C* CAD
042600070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
042700070928     C                   EXSR      CHKNUM
042800070928     C                   IF        PiInt=*on
042900070928     C                   Z-ADD     PiVal         Num5_0
043000070928     C                   MOVEL     Num5_0        VABCAD
043100070928     C                   ELSE
043200070928     C                   SETON                                        32
043300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400070928     C                             + ' ' + 'VABCAD'
043500070928     C                   ENDIF
043600071003     C* NSP/RMN
043700080513     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
043800070928     C                   EXSR      CHKNUM
043900070928     C                   IF        PiInt=*on
044000070928     C                   Z-ADD     PiVal         VABRMN
044100070928     C                   Z-ADD     PiVal         VABNSP
044200070928     C                   Z-ADD     PiVal         VATNSP
044300070928     C                   ELSE
044400070928     C                   SETON                                        31
044500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600070928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
044700070928     C                   ENDIF
044800071003     C* NCL
044900070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
045000070928     C                   EXSR      CHKNUM
045100070928     C                   IF        PiInt=*on
045200070928     C                   Z-ADD     PiVal         VABNCL
045300070928     C                   ELSE
045400070928     C                   SETON                                        32
045500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600070928     C                             + ' ' + 'VABNCL'
045700070928     C                   ENDIF
045800071003     C* PKB
045900070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
046000070928     C                   EXSR      CHKNUM
046100070928     C                   IF        PiNum=*on
046200070928     C                   Z-ADD     PiVal         VABPKB
046300070928     C                   ELSE
046400070928     C                   SETON                                        32
046500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046600070928     C                             + ' ' + 'VABPKB'
046700070928     C                   ENDIF
046800071003     C* CAS
046900070928     C                   IF        %trim(%subst(vindta:280:12))<>*blanks AND
047000070928     C                             %trim(%subst(vindta:280:12))<>*zeros  AND
047100080117     C                             %trim(%subst(vindta:280:12))<>'0.00'  AND
047200080117     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
047300070928     C                   MOVEL     '1'           FlgCAS
047400070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047500070928     C                   EXSR      CHKNUM
047600070928     C                   IF        PiNum=*on
047700070928     C                   Z-ADD     PiVal         VABCAS
047800070928     C                   ELSE
047900070928     C                   SETON                                        32
048000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100070928     C                             + ' ' + 'VABCAS'
048200070928     C                   ENDIF
048300070928     C                   ENDIF
048400071003     C* IAS
048500071003     C                   IF        %trim(%subst(vindta:295:12))<>*blanks AND
048600071003     C                             %trim(%subst(vindta:295:12))<>*zeros  AND
048700080117     C                             %trim(%subst(vindta:295:12))<>'0.00'  AND
048800080117     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
048900071003     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049000071003     C                   EXSR      CHKNUM
049100071003     C                   IF        PiNum=*on
049200071003     C                   Z-ADD     PiVal         VABIAS
049300071003     C                   MOVEL     'EUR'         VABVAS
049400071003     C                   ELSE
049500071003     C                   SETON                                        32
049600071003     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700071003     C                             + ' ' + 'VABIAS'
049800071003     C                   ENDIF
049900071003     C                   ENDIF
050000070730     C*
050100070730     C* Considerazioni finali su CBO/CAS
050200070730     C                   IF        FlgCAS = '1'
050300070730     C                   IF        VABCBO = '1'
050400070730     C                   EVAL      VABCBO = '4'
050500070730     C                   ENDIF
050600070730     C                   IF        VABCBO = '2'
050700070730     C                   EVAL      VABCBO = '6'
050800070730     C                   ENDIF
050900070730     C                   ENDIF
051000020305     C*
051100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051200011113     C                   EXSR      CHKIMPDIV
051300010202     C*
051400000801     C* Ebbene...
051500000801     C                   ADD       1             §CTRMO
051600070530     C                   IF        *in31 <> *zeros OR
051700070530     C                             *in32 <> *zeros
051800000801     C                   ADD       1             §CTRNO
051900000801     C                   EVAL      recko = vindta
052000000801     C                   EXCEPT    rigadett
052100020725     C                   EVAL      x_vinflg = '2'
052200000801     C                   ELSE
052300010201     C                   ADD       1             §CTROKVB
052400000801     C                   ENDIF
052500000801     C*
052600000801     C                   ENDSR
052700010201     C*----------------------------------------------------*
052800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052900010201     C*----------------------------------------------------*
053000071003     C     WRIVAT_B      BEGSR
053100010201     C*
053200021113     C* Valorizzo l buffer di scrittura del FIVAT
053300070928     C* - TIPO RECORD "B"
053400070928     C                   if        %subst(vindta:92:15) <> *blanks
053500070928     C                   eval      VATTRC = 'B'
053600070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
053700021113     C                   write     FIVAT000
053800020725     C                   endif
053900010201     C*
054000010201     C                   ENDSR
054100071003     C*----------------------------------------------------*
054200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054300071003     C*----------------------------------------------------*
054400071003     C     WRIVAT_E      BEGSR
054500071003     C*
054600071003     C* Valorizzo l buffer di scrittura del FIVAT
054700071003     C* - TIPO RECORD "E"
054800071003     C                   if        %subst(vindta:331:26) <> *blanks
054900071003     C                   eval      VATTRC = 'E'
055000071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
055100071003     C                   write     FIVAT000
055200071003     C                   endif
055300071003     C*
055400071003     C                   ENDSR
055500020904
055600020904
055700010202     C*----------------------------------------------------*
055800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055900010202     C*----------------------------------------------------*
056000020305     C     PREVAT        BEGSR
056100010202     C*
056200021113     C* Compongo il nome del membro da dare al FIVATWWR
056300010202     C                   eval      parmbr = vlrhdl
056400010202     C                   movel     'M'           parmbr
056500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
056600010202     C                   eval      paropz = '1'
056700010202     C* Effettuo la chiamata al CLLE preposto
056800050128     C                   call(e)   'TITVVTC'
056900010202     C                   parm                    parccm
057000010202     C                   parm                    parmbr
057100010202     C                   parm                    paropz
057200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057300010202     C                   if        %error
057400010202     C                   movel     '1'           chkcall
057500010202     C                   else
057600010202     C                   movel     '0'           chkcall
057700010202     C                   endif
057800010202     C*
057900010202     C                   ENDSR
058000000801     C*----------------------------------------------------*
058100000801     C*  CONTROLLO NUMERICITA' CAMPI
058200000801     C*----------------------------------------------------*
058300000801     C     CHKNUM        BEGSR
058400000801     C*
058500000801     C                   call(e)   'ISNUMERIC'
058600000801     C                   PARM                    PiStr            30
058700071003     C                   PARM      '.'           PiDecChr          1
058800000801     C                   PARM      *ZEROS        PiVal            30 9
058900000801     C                   PARM      '0'           PiInt             1
059000000801     C                   PARM      '0'           PiNum             1
059100000801     C                   IF        %error
059200000801     C                   EVAL      PiInt=*off
059300000801     C                   ENDIF
059400000801     C*
059500000801     C                   ENDSR
059600000801     C***
059700000801
059800011113
059900011113     C*----------------------------------------------------*
060000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060100011113     C*----------------------------------------------------*
060200011113     C     CHKIMPDIV     BEGSR
060300011113     C*
060400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060500011113     C                   Z-ADD     *zeros        wrkDec            9 9
060600011113     C*
060700011113     C* Come prima cosa effettuo considerazioni sulla divisa
060800011113     C                   IF        vabIAS > *zeros
060900011113     C                   IF        vabVAS <> 'EUR'
061000011113     C                   EVAL      vabVAS =  'ITL'
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C                   IF        vabCAS > *zeros
061500011113     C                   IF        vabVCA <> 'EUR'
061600011113     C                   EVAL      vabVCA =  'ITL'
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C                   IF        vabVMD > *zeros
062100020305     C                   IF        vabVAD <> 'EUR'
062200011113     C                   EVAL      vabVAD =  'ITL'
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062700011113     C                   Z-ADD     vabIAS        wrkDec
062800011113     C                   IF        wrkDec > *zeros
062900011113     C                   IF        vabVAS = 'ITL'
063000011113     C                   EVAL      vabIAS = *zeros
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063500011113     C                   Z-ADD     vabCAS        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVCA = 'ITL'
063800011113     C                   EVAL      vabCAS = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064300011113     C                   Z-ADD     vabVMD        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVAD = 'ITL'
064600011113     C                   EVAL      vabVMD = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C                   ENDSR
065100011113     C***
065200011113
065300011113
065400000801
065500000801
065600990920      /TITLE Invio dei dati al punto operativo.
065700010202     C     invio         BEGSR
065800990920     C*
065900021113     C* 1° invio FIVAT
066000010201     C                   reset                   dscmz
066100010201     C                   move      vlrpoi        cmzdst
066200021113     C                   eval      cmzfld = 'FIVATWWR'
066300010201     C                   eval      cmzmbd = vlrhdl
066400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066500021009     C***                if        prmfir = *blanks
066600021113     C                   eval      cmzfla = 'FIVAT00F'
066700021113     C                   eval      cmzmba = 'FIVAT00F'
066800021009     C***                else
066900021009     C***                eval      cmzfla = prmfir
067000021009     C***                eval      cmzmba = prmfir
067100021009     C***                endif
067200010201     C                   eval      cmznrr = *zeros
067300020305     C                   move      §ctrokvt      cmznrr
067400021018     C                   eval      cmzlba = vlrfl1
067500010201     C                   call(e)   'TIS711C'
067600010201     C                   parm                    dscmz
067700010201     C                   parm      *blanks       esito
067800010205     C                   if        %error
067900010205     C                             or cmzerr = '1'
068000010205     C                             or esito  = '1'
068100010205     C                   eval      wrkesito = '3'
068200010205     C                   else
068300010201     C*
068400021113     C* 2° invio FIVAB
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700010201     C                   eval      cmzfld = vlrfou
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAB00F'
069200021113     C                   eval      cmzmba = 'FIVAB00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800010201     C                   move      §ctrokvb      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010201     C                   if        %error
070400010201     C                             or cmzerr = '1'
070500010201     C                             or esito  = '1'
070600010201     C                   eval      wrkesito = '3'
070700010201     C                   endif
070800010205     C                   endif
070900990920     C*
071000000613     C                   ENDSR
071100000613     C***
071200070411
071300070411     C     *pssr         BEGSR
071400070411     C*
071500070411     C                   if        %open(tivin00r)
071600070411     C                   close     tivin00r
071700070411     C                   endif
071800070411     C                   if        %open(fivabwwr)
071900070411     C                   close     fivabwwr
072000070411     C                   endif
072100070411     C                   if        %open(fivatwwr)
072200070411     C                   close     fivatwwr
072300070411     C                   endif
072400070411     C*
072500070411     C* Effettuo la chiamata al CLLE preposto
072600070411     C                   call(e)   'TITVVTC'
072700070411     C                   parm                    parccm
072800070411     C                   parm                    parmbr
072900070411     C                   parm      '2'           paropz
073000070411     C*
073100070411     C                   eval      wrkesito = '2'
073200070411     C*
073300070411     C                   seton                                        LR
073400070411     C*
073500070411     C                   ENDSR     '*CANCL'
073600070411     C***
073700070411
073800990910
073900000613     C     *inzsr        BEGSR
074000990910     C*
074100990910     C     *entry        plist
074200990920     C                   parm                    tivlrds
074300990921     C                   parm      wrkesito      esito
074400000724     C                   parm                    prmlit
074500000710     C                   parm                    prmfir
074600000613     C*
074700000830     C* CALCOLA LA DATA CORRENTE
074800000830     C                   time                    wn14             14 0
074900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075000000830     C                   z-add     wn8           g08dat
075100000830     C                   z-add     *zeros        g08inv
075200000830     C                   movel     '0'           g08err
075300000830     C                   call      'XSRDA8'
075400000830     C                   parm                    wlbda8
075500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075600000830     C*
075700000613     C                   ENDSR
075800000613     C***
075900990908
076000080117     Otitv1d4p  E            riepilogo         2
076100990915     O                                              'Upload via Internet'
076200990915     O                                           +1 'Traduzione TIVIN00R -'
076300021113     O                                           +1 'FIVABWWR/FIVATWWR'
076400010201     O                                           +1 'Report testate bolle'
076500990915     O          E            riepilogo   2
076600990915     O                       wrkdata
076700990915     O                       wrkora              +1
076800990915     O                       procname            +1
076900990915     O          E            riepilogo   2
077000990915     O                                              'Cliente..................:'
077100990915     O                       VABCCM        z     +1
077200990915     O          E            riepilogo   2
077300990920     O                                              'Riferimento Strategi.....:'
077400990920     O                       vlrhdl              +1
077500990915     O          E            riepilogo   2
077600990915     O                                              'Giusti...................:'
077700010201     O                       §CTROKVB      2   +  1
077800990915     O          E            riepilogo   2
077900990915     O                                              'Sbagliati e corretti.....:'
078000971022     O                       §CTRMO        2   +  1
078100990915     O          E            riepilogo   2
078200990915     O                                              'Sbagliati e scartati.....:'
078300971022     O                       §CTRNO        2   +  1
078400000613
078500080117     Otitv1d4ps E            testdett          2
078600000613     O                                              'Upload via Internet'
078700000613     O                                           +1 'Traduzione TIVIN00R -'
078800021113     O                                           +1 'FIVABWWR/FIVATWWR'
078900010201     O                                           +1 'Report testate bolle'
079000000616     O          E            testdett    3
079100000613     O                                           +2 'N° rec'
079200000613     O                                           +3 'Anteprima contenuto'
079300000616     O          E            rigadett    2
079400000613     O                       rrnum               +2
079500000621     O                       recko               +3
079600000616     O          E            findett     2
079700000613     O                       wrkdata
079800000613     O                       wrkora              +1
079900000613     O                       procname            +1
080000000616     O          E            findett     2
080100000613     O                                              'Cliente..................:'
080200000613     O                       VABCCM        z     +1
080300000616     O          E            findett     2
080400000613     O                                              'Riferimento Strategi.....:'
080500000613     O                       vlrhdl              +1
080600000616     O          E            findett     2
080700000613     O                                              'Giusti...................:'
080800010201     O                       §CTROKVB      2   +  1
080900000616     O          E            findett     2
081000000613     O                                              'Sbagliati e corretti.....:'
081100000613     O                       §CTRMO        2   +  1
081200000616     O          E            findett     2
081300000613     O                                              'Sbagliati e scartati.....:'
081400000613     O                       §CTRNO        2   +  1
081500000512** CMD - COMANDI CL
081600080117OVRPRTF FILE(TITV1D4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081700080117OVRPRTF FILE(TITV1D4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081800080117DLTOVR FILE(TITV1D4P TITV1D4PS) LVL(*)
081900000512
082000000512
