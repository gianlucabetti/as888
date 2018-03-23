000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070730     Ftitvz4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070730     Ftitvz4ps  O    f  198        PRINTER usropn
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
003600070730     D depspe          s             10    INZ(*blanks)
003700010213     D depdat          s              8  0 INZ(*zeros)
003800010213     D depaas          s              4    INZ(*zeros)
003900070730     D curspe          s             10    INZ(*blanks)
004000010202     D parccm          s              8    INZ(*blanks)
004100010202     D parmbr          s             10    INZ(*blanks)
004200010202     D paropz          s              1    INZ(*blanks)
004300010202     D chkcall         s              1    INZ(*blanks)
004400020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004500070530     D Num5_0          s              5  0
004600000830
004700000830     D*------------------
004800000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900000830     D*------------------
005000000830     D WLBDA8          DS                  INZ
005100000830     D  G08DAT                 1      8  0
005200000830     D  G08INV                 9     16  0
005300000830     D  G08ERR                17     17
005400000830     D  G08TGI                18     22  0
005500000830     D*
005600020725
005700020725     D*------------------
005800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005900020725     D*------------------
006000070530     D INPUT_DS        DS                  INZ
006100020725     D  VINDTA                 1   2048
006200020725     D  VINFLG              2049   2049
006300020725     D  VINSPE              2050   2059
006400020725     D  VINRRN              2060   2067  0
006500020725     D*
006600990908
006700010201
006800071120     C*
006900071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000071120     C
007100071120     C/EXEC SQL
007200071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300071120     C/END-EXEC
007400071120     C*
007500990915     C                   time                    wrkdata
007600990915     C                   time                    wrkora
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000724     C*
008100000724     C* SE OCCORRE SPEDIRE IN FILIALE
008200000724     C                   if        vlrpoi <> *zeros
008300000724     C*
008400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000724     C     vlrpoi        chain     azorg01l
008700000724     C                   if        %found
008800000616     C                   movel(p)  CMD(1)        depcmd
008900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000724     C*
009700000724     C                   endif
009800000724     C                   endif
009900000616     C*
010000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100000616     C                   movel(p)  CMD(2)        depcmd
010200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010300000616     C*
010400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010500011113     C                   Z-ADD     150           LENGH            15 5
010600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000616     C                   PARM                    depcmd
010800000616     C                   PARM                    LENGH
010900000616     C*
011000070730     C                   if        not %open(titvz4ps)
011100070730     C                   open      titvz4ps
011200000616     C                   except    testdett
011300000613     C                   endif
011400000613     C*
011500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010202     C* Effettuo la chiamata al CLLE preposto
011900050128     C                   call(e)   'TITVVTC'
012000010202     C                   parm                    parccm
012100010202     C                   parm                    parmbr
012200010202     C                   parm      '2'           paropz
012300070730     C*
012400070730     C* Effettuo lancio TISI95 solo x chiusura
012500070730     C                   CLEAR                   TISI95DS
012600070730     C                   EVAL      I95TLA = 'C'
012700070730     C                   CALL      'TISI95R'
012800070730     C                   PARM                    TISI95DS
012900010202     C*
013000070730     C                   if        %open(titvz4ps)
013100000616     C                   except    findett
013200070730     C                   close     titvz4ps
013300000613     C                   endif
013400000616     C*
013500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013800000616     C                   PARM                    CMD(3)
013900000616     C                   PARM                    LENGH
014000000616     C*
014100000801     C
014200010201     C                   seton                                        LR
014300000613
014400000613     C*--------------------------------------------------------
014500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014600000613     C*--------------------------------------------------------
014700000621     C     STPR          BEGSR
014800000613     C*
014900070730     C                   if        not %open(titvz4p)
015000070730     C                   open      titvz4p
015100990915     C                   endif
015200990915     C*
015300990915     C                   except    riepilogo
015400990915     C*
015500070730     C                   if        %open(titvz4p)
015600070730     C                   close     titvz4p
015700990914     C                   endif
015800990910     C*
015900000613     C                   ENDSR
016000000613     C***
016100990908
016200000801
016300910830     C*--------------------------------------------------------
016400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016500910830     C*--------------------------------------------------------
016600070530     C     RWFILE        BEGSR
016700990910     C*
016800990914     C                   if        not %open(tivin00r)
016900990908     C                   open      tivin00r
017000990914     C                   endif
017100021113     C                   if        not %open(fivabwwr)
017200021113     C                   open      fivabwwr
017300990914     C                   endif
017400070530     C*
017500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017600020305     C                   exsr      prevat
017700010201     C*
017800010202     C                   if        chkcall = '0'
017900010202     C*
018000021113     C                   if        not %open(fivatwwr)
018100021113     C                   open      fivatwwr
018200010201     C                   endif
018300990910     C*
018400010201     C                   clear                   §CTROKVB          5 0
018500020305     C                   clear                   §CTROKVT          5 0
018600000801     C                   clear                   §CTRMO            5 0
018700000801     C                   clear                   §CTRNO            5 0
018800990910     C*
018900020725     C/EXEC SQL
019000020725     C+ declare C1 cursor for select
019100070730     C+ vindta, vinflg, substr(vindta, 3, 10) as sped, rrn(tivin00r)
019200060223     C+ from tivin00r
019300060223     C+ order by sped
019400020725     C+ for read only
019500020725     C/END-EXEC
019600020725     C
019700020725     C/EXEC SQL
019800020725     C+ open C1
019900020725     C/END-EXEC
020000020725     C
020100020725     C/EXEC SQL
020200070530     C+ Fetch C1 into :INPUT_DS
020300020725     C/END-EXEC
020400020725     C*
020500020725     C                   dow       sqlcod = *zeros
020600990913     C*
020700020725     C                   if        vindta > *blanks
020800000613     C                   add       1             rrnum
020900000801     C*
021000020725     C                   if        vinflg = *blanks
021100020725     C                             or vinflg = '0'
021200020725     C                             or vinflg = '2'
021300000801     C*
021400020725     C                   clear                   x_vinmsg
021500020725     C                   eval      x_vinflg = '1'
021600010305     C*
021700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021800070730     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
021900020305     C                   MOVEL(p)  PiStr         curspe
022000010305     C*
022100010305     C                   if        depspe = *blanks                             => 1° giro
022200010305     C                   eval      depspe = curspe                              => memorizz. spediz
022300020725     C                   clear                   tivinds
022400020725     C                   exsr      inzvar
022500020725     C                   exsr      defcam
022600020305     C                   exsr      impvab
022700070530     C                   exsr      wrivat                                       => carico VAT
022800010305     C                   else
022900020725     C                   if        depspe <> curspe                             => rottura di spediz
023000010305     C                   eval      depspe = curspe                              => memorizz. spediz
023100020305     C                   exsr      wrivab
023200020725     C                   clear                   tivinds
023300020725     C                   exsr      inzvar
023400020725     C                   exsr      defcam
023500020305     C                   exsr      impvab
023600070530     C                   exsr      wrivat                                       => carico VAT
023700020305     C                   else                                                   => x stessa spediz
023800020305     C                   exsr      impvab
023900070530     C                   exsr      wrivat                                       => carico VAT
024000010305     C                   endif
024100010305     C                   endif
024200010305     C                   endif
024300000905     C*
024400000905     C                   else
024500020725     C                   eval      x_vinflg = '1'
024600000905     C                   endif
024700000905     C*
024800020725     C     VINRRN        chain     tivin000
024900020725     C                   if        %found(tivin00r)
025000020725     C                   eval      y_vinflg = x_vinflg
025100020725     C                   eval      y_vinmsg = x_vinmsg
025200020725     C                   update    tivin000
025300020725     C                   endif
025400020725     C*
025500020725     C/EXEC SQL
025600070530     C+ Fetch C1 into :INPUT_DS
025700020725     C/END-EXEC
025800020725     C*
025900020725     C                   enddo
026000020725     C*
026100020725     C/EXEC SQL
026200020725     C+ close C1
026300020725     C/END-EXEC
026400000905     C*
026500020305     C* Scarico i VAB rimasti "in sospeso"
026600020305     C                   exsr      wrivab
026700010202     C*
026800010202     C                   endif
026900990910
027000990910     C* Se non ci sono record con errori ...
027100000710     C                   if        §ctrno = 0
027200990910     C* ... restituisco esito OK.
027300990921     C                   eval      wrkesito = '0'
027400990910     C                   else
027500010201     C                   if        §ctrokvb > 0
027600990921     C                   eval      wrkesito = '1'
027700000710     C                   else
027800000710     C                   eval      wrkesito = '2'
027900990910     C                   endif
028000000710     C                   endif
028100990910     C*
028200990914     C                   if        %open(tivin00r)
028300990908     C                   close     tivin00r
028400990914     C                   endif
028500021113     C                   if        %open(fivabwwr)
028600021113     C                   close     fivabwwr
028700990914     C                   endif
028800021113     C                   if        %open(fivatwwr)
028900021113     C                   close     fivatwwr
029000010201     C                   endif
029100990910     C*
029200010201     C                   if        §ctrokvb > 0
029300000724     C                             and vlrpoi <> *zeros
029400010202     C                   exsr      invio
029500990920     C                   endif
029600990920     C*
029700910830     C                   ENDSR
029800000613     C***
029900010305
030000010305     C*----------------------------------------------------*
030100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030200010305     C*----------------------------------------------------*
030300020305     C     WRIVAB        BEGSR
030400010305     C*
030500070730     C* Forzatura in caso d mancanza peso dal cliente => fisso 20 Kg.
030600070803     C                   if        vabpkb = *zeros AND vabccm <> 0063657
030700070730     C                   eval      vabpkb = 20 * vabncl
030800070730     C                   endif
030900070803     C                   if        vabpkb = *zeros AND vabccm  = 0063657
031000070803     C                   eval      vabpkb = 15 * vabncl
031100070803     C                   endif
031200070730     C*
031300021113     C                   write     fivab000                                     => scarico il VAB
031400070530     C*
031500070530     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A e B
031600070530     C                   if        wVAT_A <> *blanks
031700070530     C                   eval      VATTRC = 'A'
031800070530     C                   eval      VATNOT = wVAT_A
031900070530     C                   write     FIVAT000
032000070530     C                   endif
032100070530     C*
032200070530     C                   if        wVAT_B <> *blanks
032300070530     C                   eval      VATTRC = 'B'
032400070530     C                   eval      VATNOT = wVAT_B
032500070530     C                   write     FIVAT000
032600070530     C                   endif
032700010305     C*
032800010305     C                   ENDSR
032900990920
033000000801     C*----------------------------------------------------*
033100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033200000801     C*----------------------------------------------------*
033300010201     C     INZVAR        BEGSR
033400000801     C*
033500010201     C                   Z-ADD     *zeros        Num5_0
033600070530     C                   MOVEL     *blanks       wVAT_A           30
033700070530     C                   MOVEL     *blanks       wVAT_B           30
033800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033900020725     C                   MOVEL     '0'           FlgCAS            1
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034400000801     C*----------------------------------------------------*
034500020904     C     DEFCAM        BEGSR
034600000801     C*
034700021113     C                   CLEAR                   FIVAB000
034800021113     C                   CLEAR                   FIVAT000
034900070730     C* Imposto i valori di default...
035000070730     C                   EVAL      VABCCM = 0063657
035100070730     C                   EVAL      VATCCM = 0063657
035200070730     C                   EVAL      VABLNP = 006
035300070730     C                   EVAL      VATLNP = 006
035400070730     C                   EVAL      VABCTR = 100
035500070730     C                   EVAL      VABCBO = '1'
035600020305     C                   MOVEL     '7Q'          VABCTM
035700070530     C                   MOVEL     '1'           VABCBO
035800070222     C* ... e poi verifico se sono stati passati come parametri
035900070222     C                   IF        vlrppt > *blanks
036000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCCM
036500070222     C                   Z-ADD     PiVal         VATCCM
036600070222     C                   ENDIF
036700070222     C                   ENDIF
036800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037000070222     C                   EXSR      CHKNUM
037100070222     C                   IF        PiInt=*on
037200070222     C                   Z-ADD     PiVal         VABLNP
037300070222     C                   Z-ADD     PiVal         VATLNP
037400070222     C                   ENDIF
037500070222     C                   ENDIF
037600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037800070222     C                   EXSR      CHKNUM
037900070222     C                   IF        PiInt=*on
038000070222     C                   Z-ADD     PiVal         VABCTR
038100070222     C                   ENDIF
038200070222     C                   ENDIF
038300070222     C                   ENDIF
038400000801     C*
038500000801     C                   ENDSR
038600000801     C*----------------------------------------------------*
038700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
038800000801     C*----------------------------------------------------*
038900010201     C     IMPVAB        BEGSR
039000070530     C*
039100070530     C                   SETOFF                                       3132
039200021113     C*
039300021113     C                   MOVE(P)   vlrpoi        VABFGS
039400021113     C                   MOVE(P)   vlrpoi        VATFGS
039500020725     C*
039600070530     C* Campi anno e mese/giorno
039700070530     C                   MOVEL     datcor        VABAAS
039800070530     C                   MOVEL     datcor        VATAAS
039900070530     C                   MOVE      datcor        VABMGS
040000070730     C*
040100070730     C* Reperimento campi ALFA
040200070730     C*
040300070730     C* Considerazioni sulla ragione sociale del destinatario da indicare
040400070730     C                   EVAL      VABRSD=%trim(%subst(vindta:88:30))
040500070730     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
040600070730     C     '@':'A'       XLATE     VABRSD        VABRSD
040700070730     C* ==
040800070730     C                   EVAL      VABIND=%trim(%subst(vindta:118:30))
040900070730     C                   EVAL      VABLOD=%trim(%subst(vindta:148:30))
041000070730     C                   EVAL      VABPRD=%trim(%subst(vindta:183:2))
041100071120     C                   MOVEL     *blanks       wNOTE            40
041200071120     C                   EVAL      wNOTE=%trim(%subst(vindta:253:40))
041300071120     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
041400071120     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:5))
041500070730     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
041600070730     C*
041700070730     C* Reperimento campi NUMERICI
041800070730     C                   MOVEL     DATCOR        VABAAS
041900070730     C                   MOVE      DATCOR        VABMGS
042000070730     C* CCM
042100070730     C                   IF        %subst(vindta:3:1) = 'C' OR
042200070730     C                             %subst(vindta:3:1) = 'F'
042300070730     C                   EVAL      VABCCM = 0063658
042400070731     C                   EVAL      VATCCM = 0063658
042500070730     C                   EVAL      VABCTR = 100
042600070731     C                   EVAL      VABLNP = 006
042700070731     C                   EVAL      VATLNP = 006
042800070730     C                   ENDIF
042900070730     C                   IF        %subst(vindta:3:1) = 'D'
043000070730     C                   EVAL      VABCCM = 0063657
043100070731     C                   EVAL      VATCCM = 0063657
043200070730     C                   EVAL      VABCTR = 110
043300070731     C                   EVAL      VABLNP = 006
043400070731     C                   EVAL      VATLNP = 006
043500070730     C                   ENDIF
043600070730     C* NSP/RMN
043700070730     C                   EVAL      PiStr=%trim(%subst(vindta:6:5))
043800070730     C                   EXSR      CHKNUM
043900070730     C                   IF        PiInt=*on
044000070730     C                   Z-ADD     PiVal         VABNSP
044100070730     C                   Z-ADD     PiVal         VATNSP
044200070730     C                   Z-ADD     PiVal         VABRMN
044300070730     C                   ELSE
044400070730     C                   SETON                                        31
044500070730     C                   Z-ADD     *zeros        VABNSP
044600070730     C                   Z-ADD     *zeros        VATNSP
044700070730     C                   Z-ADD     1             VABRMN
044800070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044900070730     C                             + ' ' + 'VABNSP VATNSP VABRMN'
045000070730     C                   ENDIF
045100070730     C* CAD
045200070730     C                   EVAL      PiStr=%trim(%subst(vindta:178:5))
045300070730     C                   EXSR      CHKNUM
045400070730     C                   IF        PiInt=*on
045500070730     C                   Z-ADD     PiVal         Num5_0
045600070730     C                   MOVEL(p)  Num5_0        VABCAD
045700070730     C                   ELSE
045800070730     C                   SETON                                        32
045900070730     C                   EVAL      VABCAD = *zeros
046000070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100070730     C                             + ' ' + 'VABCAD'
046200070730     C                   ENDIF
046300070730     C* Reperisco la provincia dal CAP e dalla località
046400070730     C                   IF        VABCAD <> *blanks AND
046500070730     C                             VABPRD  = *blanks
046600070730     C                   CLEAR                   TISI95DS
046700070730     C                   EVAL      I95TCN = '3'
046800070730     C                   Z-ADD     datcor        I95DAT
046900070730     C                   EVAL      I95CAP = VABCAD
047000070730     C                   EVAL      I95LOC = VABLOD
047100070730     C                   EVAL      I95NAR = VABNZD
047200070730     C                   CALL      'TISI95R'
047300070730     C                   PARM                    TISI95DS
047400070730     C                   EVAL      VABPRD = O95PRV
047500070730     C                   ENDIF
047600070730     C* NCL
047700070730     C                   EVAL      PiStr=%trim(%subst(vindta:192:5))
047800070730     C                   EXSR      CHKNUM
047900070730     C                   IF        PiInt=*on
048000070730     C                   Z-ADD     PiVal         VABNCL
048100070730     C                   ELSE
048200070730     C                   SETON                                        32
048300070730     C                   Z-ADD     *zeros        VABNCL
048400070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048500070730     C                             + ' ' + 'VABNCL'
048600070730     C                   ENDIF
048700070730     C* PKB
048800070730     C                   EVAL      PiStr=%trim(%subst(vindta:185:7))
048900070730     C                   EVAL      PiDecChr = '.'
049000070730     C                   EXSR      CHKNUM
049100070730     C                   IF        PiNum=*on
049200070730     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
049300070730     C                   Z-ADD     PiVal         VABPKB
049400070730     C                   ELSE
049500070730     C                   SETON                                        32
049600070730     C                   Z-ADD     *zeros        VABPKB
049700070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800070730     C                             + ' ' + 'VABPKB'
049900070730     C                   ENDIF
050000070730     C* CAS
050100070730     C                   IF        %trim(%subst(vindta:296:11)) <> *all'0'
050200070730     C                   EVAL      VABVCA = %trim(%subst(vindta:293:3))
050300070730     C                   EVAL      FlgCAS = '1'
050400070730     C                   EVAL      VABTIC = 'BM'
050500070730     C                   EVAL      PiStr=%trim(%subst(vindta:296:11))
050600070730     C                   EXSR      CHKNUM
050700070730     C                   IF        PiNum=*on
050800070730     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
050900070730     C                   Z-ADD     PiVal         VABCAS
051000070730     C                   ELSE
051100070730     C                   SETON                                        32
051200070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300070730     C                             + ' ' + 'VABCAS'
051400070730     C                   ENDIF
051500070730     C                   ENDIF
051600071204     C*
051700071204     C* Verifico se richiesta consegna x appuntamento
051800071204     C                   IF        %scan('PS#': wNOTE) > *zeros OR
051900071204     C                             %scan('ps#': wNOTE) > *zeros
052000071204     C                   EVAL      VABTC1 = 'A'
052100071204     C                   ENDIF
052200071219     C*
052300071219     C* Verifico se richiesto incasso contrassegno TM
052400071219     C                   IF        %scan('TM#': wNOTE) > *zeros OR
052500071219     C                             %scan('tm#': wNOTE) > *zeros
052600071219     C                   EVAL      VABTIC = 'BM'
052700071219     C                   ENDIF
052800070730     C*
052900070730     C* Considerazioni finali su CBO/CAS
053000070730     C                   IF        FlgCAS = '1'
053100070730     C                   IF        VABCBO = '1'
053200070730     C                   EVAL      VABCBO = '4'
053300070730     C                   ENDIF
053400070730     C                   IF        VABCBO = '2'
053500070730     C                   EVAL      VABCBO = '6'
053600070730     C                   ENDIF
053700070730     C                   ENDIF
053800020305     C*
053900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054000011113     C                   EXSR      CHKIMPDIV
054100010202     C*
054200000801     C* Ebbene...
054300000801     C                   ADD       1             §CTRMO
054400070530     C                   IF        *in31 <> *zeros OR
054500070530     C                             *in32 <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700000801     C                   EVAL      recko = vindta
054800000801     C                   EXCEPT    rigadett
054900020725     C                   EVAL      x_vinflg = '2'
055000000801     C                   ELSE
055100010201     C                   ADD       1             §CTROKVB
055200000801     C                   ENDIF
055300000801     C*
055400000801     C                   ENDSR
055500010201     C*----------------------------------------------------*
055600021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055700010201     C*----------------------------------------------------*
055800020305     C     WRIVAT        BEGSR
055900010201     C*
056000021113     C* Valorizzo l buffer di scrittura del FIVAT
056100070730     C                   if        %subst(vindta:357:10) <> *blanks
056200070530     C                   eval      VATTRC = 'E'
056300070730     C                   eval      VATNOT = %trim(%subst(vindta:357:10))
056400021113     C                   write     FIVAT000
056500020725     C                   endif
056600010201     C*
056700010201     C                   ENDSR
056800020904
056900020904
057000010202     C*----------------------------------------------------*
057100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057200010202     C*----------------------------------------------------*
057300020305     C     PREVAT        BEGSR
057400010202     C*
057500021113     C* Compongo il nome del membro da dare al FIVATWWR
057600010202     C                   eval      parmbr = vlrhdl
057700010202     C                   movel     'M'           parmbr
057800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057900010202     C                   eval      paropz = '1'
058000010202     C* Effettuo la chiamata al CLLE preposto
058100050128     C                   call(e)   'TITVVTC'
058200010202     C                   parm                    parccm
058300010202     C                   parm                    parmbr
058400010202     C                   parm                    paropz
058500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058600010202     C                   if        %error
058700010202     C                   movel     '1'           chkcall
058800010202     C                   else
058900010202     C                   movel     '0'           chkcall
059000010202     C                   endif
059100010202     C*
059200010202     C                   ENDSR
059300000801     C*----------------------------------------------------*
059400000801     C*  CONTROLLO NUMERICITA' CAMPI
059500000801     C*----------------------------------------------------*
059600000801     C     CHKNUM        BEGSR
059700000801     C*
059800000801     C                   call(e)   'ISNUMERIC'
059900000801     C                   PARM                    PiStr            30
060000070530     C                   PARM      '.'           PiDecChr          1
060100000801     C                   PARM      *ZEROS        PiVal            30 9
060200000801     C                   PARM      '0'           PiInt             1
060300000801     C                   PARM      '0'           PiNum             1
060400000801     C                   IF        %error
060500000801     C                   EVAL      PiInt=*off
060600000801     C                   ENDIF
060700000801     C*
060800000801     C                   ENDSR
060900000801     C***
061000000801
061100011113
061200011113     C*----------------------------------------------------*
061300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061400011113     C*----------------------------------------------------*
061500011113     C     CHKIMPDIV     BEGSR
061600011113     C*
061700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061800011113     C                   Z-ADD     *zeros        wrkDec            9 9
061900011113     C*
062000011113     C* Come prima cosa effettuo considerazioni sulla divisa
062100011113     C                   IF        vabIAS > *zeros
062200011113     C                   IF        vabVAS <> 'EUR'
062300011113     C                   EVAL      vabVAS =  'ITL'
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C                   IF        vabCAS > *zeros
062800011113     C                   IF        vabVCA <> 'EUR'
062900011113     C                   EVAL      vabVCA =  'ITL'
063000011113     C                   ENDIF
063100011113     C                   ENDIF
063200011113     C*
063300011113     C                   IF        vabVMD > *zeros
063400020305     C                   IF        vabVAD <> 'EUR'
063500011113     C                   EVAL      vabVAD =  'ITL'
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064000011113     C                   Z-ADD     vabIAS        wrkDec
064100011113     C                   IF        wrkDec > *zeros
064200011113     C                   IF        vabVAS = 'ITL'
064300011113     C                   EVAL      vabIAS = *zeros
064400011113     C                   ENDIF
064500011113     C                   ENDIF
064600011113     C*
064700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064800011113     C                   Z-ADD     vabCAS        wrkDec
064900011113     C                   IF        wrkDec > *zeros
065000011113     C                   IF        vabVCA = 'ITL'
065100011113     C                   EVAL      vabCAS = *zeros
065200011113     C                   ENDIF
065300011113     C                   ENDIF
065400011113     C*
065500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065600011113     C                   Z-ADD     vabVMD        wrkDec
065700011113     C                   IF        wrkDec > *zeros
065800011113     C                   IF        vabVAD = 'ITL'
065900011113     C                   EVAL      vabVMD = *zeros
066000011113     C                   ENDIF
066100011113     C                   ENDIF
066200011113     C*
066300011113     C                   ENDSR
066400011113     C***
066500011113
066600011113
066700000801
066800000801
066900990920      /TITLE Invio dei dati al punto operativo.
067000010202     C     invio         BEGSR
067100990920     C*
067200021113     C* 1° invio FIVAT
067300010201     C                   reset                   dscmz
067400010201     C                   move      vlrpoi        cmzdst
067500021113     C                   eval      cmzfld = 'FIVATWWR'
067600010201     C                   eval      cmzmbd = vlrhdl
067700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067800021009     C***                if        prmfir = *blanks
067900021113     C                   eval      cmzfla = 'FIVAT00F'
068000021113     C                   eval      cmzmba = 'FIVAT00F'
068100021009     C***                else
068200021009     C***                eval      cmzfla = prmfir
068300021009     C***                eval      cmzmba = prmfir
068400021009     C***                endif
068500010201     C                   eval      cmznrr = *zeros
068600020305     C                   move      §ctrokvt      cmznrr
068700021018     C                   eval      cmzlba = vlrfl1
068800010201     C                   call(e)   'TIS711C'
068900010201     C                   parm                    dscmz
069000010201     C                   parm      *blanks       esito
069100010205     C                   if        %error
069200010205     C                             or cmzerr = '1'
069300010205     C                             or esito  = '1'
069400010205     C                   eval      wrkesito = '3'
069500010205     C                   else
069600010201     C*
069700021113     C* 2° invio FIVAB
069800010201     C                   reset                   dscmz
069900010201     C                   move      vlrpoi        cmzdst
070000010201     C                   eval      cmzfld = vlrfou
070100010201     C                   eval      cmzmbd = vlrhdl
070200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070300021009     C***                if        prmfir = *blanks
070400021113     C                   eval      cmzfla = 'FIVAB00F'
070500021113     C                   eval      cmzmba = 'FIVAB00F'
070600021009     C***                else
070700021009     C***                eval      cmzfla = prmfir
070800021009     C***                eval      cmzmba = prmfir
070900021009     C***                endif
071000010201     C                   eval      cmznrr = *zeros
071100010201     C                   move      §ctrokvb      cmznrr
071200021018     C                   eval      cmzlba = vlrfl1
071300010201     C                   call(e)   'TIS711C'
071400010201     C                   parm                    dscmz
071500010201     C                   parm      *blanks       esito
071600010201     C                   if        %error
071700010201     C                             or cmzerr = '1'
071800010201     C                             or esito  = '1'
071900010201     C                   eval      wrkesito = '3'
072000010201     C                   endif
072100010205     C                   endif
072200990920     C*
072300000613     C                   ENDSR
072400000613     C***
072500070411
072600070411     C     *pssr         BEGSR
072700070411     C*
072800070411     C                   if        %open(tivin00r)
072900070411     C                   close     tivin00r
073000070411     C                   endif
073100070411     C                   if        %open(fivabwwr)
073200070411     C                   close     fivabwwr
073300070411     C                   endif
073400070411     C                   if        %open(fivatwwr)
073500070411     C                   close     fivatwwr
073600070411     C                   endif
073700070411     C*
073800070411     C* Effettuo la chiamata al CLLE preposto
073900070411     C                   call(e)   'TITVVTC'
074000070411     C                   parm                    parccm
074100070411     C                   parm                    parmbr
074200070411     C                   parm      '2'           paropz
074300070411     C*
074400070411     C                   eval      wrkesito = '2'
074500070411     C*
074600070411     C                   seton                                        LR
074700070411     C*
074800070411     C                   ENDSR     '*CANCL'
074900070411     C***
075000070411
075100990910
075200000613     C     *inzsr        BEGSR
075300990910     C*
075400990910     C     *entry        plist
075500990920     C                   parm                    tivlrds
075600990921     C                   parm      wrkesito      esito
075700000724     C                   parm                    prmlit
075800000710     C                   parm                    prmfir
075900000613     C*
076000000830     C* CALCOLA LA DATA CORRENTE
076100000830     C                   time                    wn14             14 0
076200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
076300000830     C                   z-add     wn8           g08dat
076400000830     C                   z-add     *zeros        g08inv
076500000830     C                   movel     '0'           g08err
076600000830     C                   call      'XSRDA8'
076700000830     C                   parm                    wlbda8
076800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076900000830     C*
077000000613     C                   ENDSR
077100000613     C***
077200990908
077300070730     Otitvz4p   E            riepilogo         2
077400990915     O                                              'Upload via Internet'
077500990915     O                                           +1 'Traduzione TIVIN00R -'
077600021113     O                                           +1 'FIVABWWR/FIVATWWR'
077700010201     O                                           +1 'Report testate bolle'
077800990915     O          E            riepilogo   2
077900990915     O                       wrkdata
078000990915     O                       wrkora              +1
078100990915     O                       procname            +1
078200990915     O          E            riepilogo   2
078300990915     O                                              'Cliente..................:'
078400990915     O                       VABCCM        z     +1
078500990915     O          E            riepilogo   2
078600990920     O                                              'Riferimento Strategi.....:'
078700990920     O                       vlrhdl              +1
078800990915     O          E            riepilogo   2
078900990915     O                                              'Giusti...................:'
079000010201     O                       §CTROKVB      2   +  1
079100990915     O          E            riepilogo   2
079200990915     O                                              'Sbagliati e corretti.....:'
079300971022     O                       §CTRMO        2   +  1
079400990915     O          E            riepilogo   2
079500990915     O                                              'Sbagliati e scartati.....:'
079600971022     O                       §CTRNO        2   +  1
079700000613
079800070730     Otitvz4ps  E            testdett          2
079900000613     O                                              'Upload via Internet'
080000000613     O                                           +1 'Traduzione TIVIN00R -'
080100021113     O                                           +1 'FIVABWWR/FIVATWWR'
080200010201     O                                           +1 'Report testate bolle'
080300000616     O          E            testdett    3
080400000613     O                                           +2 'N° rec'
080500000613     O                                           +3 'Anteprima contenuto'
080600000616     O          E            rigadett    2
080700000613     O                       rrnum               +2
080800000621     O                       recko               +3
080900000616     O          E            findett     2
081000000613     O                       wrkdata
081100000613     O                       wrkora              +1
081200000613     O                       procname            +1
081300000616     O          E            findett     2
081400000613     O                                              'Cliente..................:'
081500000613     O                       VABCCM        z     +1
081600000616     O          E            findett     2
081700000613     O                                              'Riferimento Strategi.....:'
081800000613     O                       vlrhdl              +1
081900000616     O          E            findett     2
082000000613     O                                              'Giusti...................:'
082100010201     O                       §CTROKVB      2   +  1
082200000616     O          E            findett     2
082300000613     O                                              'Sbagliati e corretti.....:'
082400000613     O                       §CTRMO        2   +  1
082500000616     O          E            findett     2
082600000613     O                                              'Sbagliati e scartati.....:'
082700000613     O                       §CTRNO        2   +  1
082800000512** CMD - COMANDI CL
082900070730OVRPRTF FILE(TITVZ4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
083000070730OVRPRTF FILE(TITVZ4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
083100070730DLTOVR FILE(TITVZ4P TITVZ4PS) LVL(*)
083200000512
083300000512
