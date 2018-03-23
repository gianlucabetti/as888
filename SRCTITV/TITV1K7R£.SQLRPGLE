000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081203     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800081203     Ftitv1k7p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100081203     Ftitv1k7ps O    f  198        PRINTER usropn
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
003600081203     D depspe          s              9    INZ(*blanks)
003700081203     D curspe          s              9    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
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
005900081203     D  VINDTA                     2048
006000081203     D  VINFLG                        1
006100081203     D  VINSPE                        9
006200081203     D  VINRRN                        8  0
006300020725     D*
006400081203
006500081203
006600081203     D*------------------
006700081203     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006800081203     D*------------------
006900081203     D PiStr           S            100A   INZ
007000081203     D PiDecChr        S              1A   INZ
007100081203     D PiVal           S             63S30 INZ
007200081203     D PiInt           S               N   INZ
007300081203     D PiNum           S               N   INZ
007400081203
007500081203     D*------------------
007600081203     D* LINKING A DEFINIZIONI ESTERNE
007700081203     D*------------------
007800081203     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007900081203
008000081203
008100990908
008200010201
008300071120     C*
008400071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008500071120     C
008600071120     C/EXEC SQL
008700071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008800071120     C/END-EXEC
008900071120     C*
009000990915     C                   time                    wrkdata
009100990915     C                   time                    wrkora
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500000724     C*
009600000724     C* SE OCCORRE SPEDIRE IN FILIALE
009700000724     C                   if        vlrpoi <> *zeros
009800000724     C*
009900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100000724     C     vlrpoi        chain     azorg01l
010200000724     C                   if        %found
010300000616     C                   movel(p)  CMD(1)        depcmd
010400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010500000616     C*
010600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    depcmd
011000000616     C                   PARM                    LENGH
011100000724     C*
011200000724     C                   endif
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400000616     C*
012500081203     C                   if        not %open(titv1k7ps)
012600081203     C                   open      titv1k7ps
012700000616     C                   except    testdett
012800000613     C                   endif
012900000613     C*
013000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013200000613     C*
013300010202     C* Effettuo la chiamata al CLLE preposto
013400050128     C                   call(e)   'TITVVTC'
013500010202     C                   parm                    parccm
013600010202     C                   parm                    parmbr
013700010202     C                   parm      '2'           paropz
013800070730     C*
013900070730     C* Effettuo lancio TISI95 solo x chiusura
014000070730     C                   CLEAR                   TISI95DS
014100070730     C                   EVAL      I95TLA = 'C'
014200070730     C                   CALL      'TISI95R'
014300070730     C                   PARM                    TISI95DS
014400010202     C*
014500081203     C                   if        %open(titv1k7ps)
014600000616     C                   except    findett
014700081203     C                   close     titv1k7ps
014800000613     C                   endif
014900000616     C*
015000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    CMD(3)
015400000616     C                   PARM                    LENGH
015500000616     C*
015600000801     C
015700010201     C                   seton                                        LR
015800000613
015900000613     C*--------------------------------------------------------
016000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016100000613     C*--------------------------------------------------------
016200000621     C     STPR          BEGSR
016300000613     C*
016400081203     C                   if        not %open(titv1k7p)
016500081203     C                   open      titv1k7p
016600990915     C                   endif
016700990915     C*
016800990915     C                   except    riepilogo
016900990915     C*
017000081203     C                   if        %open(titv1k7p)
017100081203     C                   close     titv1k7p
017200990914     C                   endif
017300990910     C*
017400000613     C                   ENDSR
017500000613     C***
017600990908
017700000801
017800910830     C*--------------------------------------------------------
017900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018000910830     C*--------------------------------------------------------
018100070530     C     RWFILE        BEGSR
018200990910     C*
018300990914     C                   if        not %open(tivin00r)
018400990908     C                   open      tivin00r
018500990914     C                   endif
018600021113     C                   if        not %open(fivabwwr)
018700021113     C                   open      fivabwwr
018800990914     C                   endif
018900070530     C*
019000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019100020305     C                   exsr      prevat
019200010201     C*
019300010202     C                   if        chkcall = '0'
019400010202     C*
019500021113     C                   if        not %open(fivatwwr)
019600021113     C                   open      fivatwwr
019700010201     C                   endif
019800990910     C*
019900010201     C                   clear                   §CTROKVB          5 0
020000020305     C                   clear                   §CTROKVT          5 0
020100000801     C                   clear                   §CTRMO            5 0
020200000801     C                   clear                   §CTRNO            5 0
020300990910     C*
020400020725     C/EXEC SQL
020500020725     C+ declare C1 cursor for select
020600081203     C+ vindta, vinflg, substr(vindta, 123, 9) as sped, rrn(tivin00r)
020700060223     C+ from tivin00r
020800060223     C+ order by sped
020900020725     C+ for read only
021000020725     C/END-EXEC
021100020725     C
021200020725     C/EXEC SQL
021300020725     C+ open C1
021400020725     C/END-EXEC
021500020725     C
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   dow       sqlcod = *zeros
022100990913     C*
022200020725     C                   if        vindta > *blanks
022300000613     C                   add       1             rrnum
022400000801     C*
022500020725     C                   if        vinflg = *blanks
022600020725     C                             or vinflg = '0'
022700020725     C                             or vinflg = '2'
022800000801     C*
022900020725     C                   clear                   x_vinmsg
023000020725     C                   eval      x_vinflg = '1'
023100010305     C*
023200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023300081203     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
023400020305     C                   MOVEL(p)  PiStr         curspe
023500010305     C*
023600010305     C                   if        depspe = *blanks                             => 1° giro
023700010305     C                   eval      depspe = curspe                              => memorizz. spediz
023800020725     C                   clear                   tivinds
023900020725     C                   exsr      inzvar
024000020725     C                   exsr      defcam
024100020305     C                   exsr      impvab
024200081203     C                   exsr      wrivate                                      => carico VAT
024300010305     C                   else
024400020725     C                   if        depspe <> curspe                             => rottura di spediz
024500010305     C                   eval      depspe = curspe                              => memorizz. spediz
024600020305     C                   exsr      wrivab
024700020725     C                   clear                   tivinds
024800020725     C                   exsr      inzvar
024900020725     C                   exsr      defcam
025000020305     C                   exsr      impvab
025100081203     C                   exsr      wrivate                                      => carico VAT
025200020305     C                   else                                                   => x stessa spediz
025300020305     C                   exsr      impvab
025400081203     C                   exsr      wrivate                                      => carico VAT
025500010305     C                   endif
025600010305     C                   endif
025700010305     C                   endif
025800000905     C*
025900000905     C                   else
026000020725     C                   eval      x_vinflg = '1'
026100000905     C                   endif
026200000905     C*
026300020725     C     VINRRN        chain     tivin000
026400020725     C                   if        %found(tivin00r)
026500020725     C                   eval      y_vinflg = x_vinflg
026600020725     C                   eval      y_vinmsg = x_vinmsg
026700020725     C                   update    tivin000
026800020725     C                   endif
026900020725     C*
027000020725     C/EXEC SQL
027100070530     C+ Fetch C1 into :INPUT_DS
027200020725     C/END-EXEC
027300020725     C*
027400020725     C                   enddo
027500020725     C*
027600020725     C/EXEC SQL
027700020725     C+ close C1
027800020725     C/END-EXEC
027900000905     C*
028000020305     C* Scarico i VAB rimasti "in sospeso"
028100020305     C                   exsr      wrivab
028200010202     C*
028300010202     C                   endif
028400990910
028500990910     C* Se non ci sono record con errori ...
028600000710     C                   if        §ctrno = 0
028700990910     C* ... restituisco esito OK.
028800990921     C                   eval      wrkesito = '0'
028900990910     C                   else
029000010201     C                   if        §ctrokvb > 0
029100990921     C                   eval      wrkesito = '1'
029200000710     C                   else
029300000710     C                   eval      wrkesito = '2'
029400990910     C                   endif
029500000710     C                   endif
029600990910     C*
029700990914     C                   if        %open(tivin00r)
029800990908     C                   close     tivin00r
029900990914     C                   endif
030000021113     C                   if        %open(fivabwwr)
030100021113     C                   close     fivabwwr
030200990914     C                   endif
030300021113     C                   if        %open(fivatwwr)
030400021113     C                   close     fivatwwr
030500010201     C                   endif
030600990910     C*
030700010201     C                   if        §ctrokvb > 0
030800000724     C                             and vlrpoi <> *zeros
030900010202     C                   exsr      invio
031000990920     C                   endif
031100990920     C*
031200910830     C                   ENDSR
031300000613     C***
031400010305
031500010305     C*----------------------------------------------------*
031600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031700010305     C*----------------------------------------------------*
031800020305     C     WRIVAB        BEGSR
031900070730     C*
032000021113     C                   write     fivab000                                     => scarico il VAB
032100070530     C*
032200070530     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A e B
032300070530     C                   if        wVAT_A <> *blanks
032400070530     C                   eval      VATTRC = 'A'
032500070530     C                   eval      VATNOT = wVAT_A
032600070530     C                   write     FIVAT000
032700070530     C                   endif
032800070530     C*
032900070530     C                   if        wVAT_B <> *blanks
033000070530     C                   eval      VATTRC = 'B'
033100070530     C                   eval      VATNOT = wVAT_B
033200070530     C                   write     FIVAT000
033300070530     C                   endif
033400010305     C*
033500010305     C                   ENDSR
033600990920
033700000801     C*----------------------------------------------------*
033800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033900000801     C*----------------------------------------------------*
034000010201     C     INZVAR        BEGSR
034100000801     C*
034200010201     C                   Z-ADD     *zeros        Num5_0
034300070530     C                   MOVEL     *blanks       wVAT_A           30
034400070530     C                   MOVEL     *blanks       wVAT_B           30
034500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034600020725     C                   MOVEL     '0'           FlgCAS            1
034700000801     C*
034800000801     C                   ENDSR
034900000801     C*----------------------------------------------------*
035000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
035100000801     C*----------------------------------------------------*
035200020904     C     DEFCAM        BEGSR
035300000801     C*
035400021113     C                   CLEAR                   FIVAB000
035500021113     C                   CLEAR                   FIVAT000
035600070730     C* Imposto i valori di default...
035700081203     C                   EVAL      VABCCM = 0633192
035800081203     C                   EVAL      VATCCM = 0633192
035900081204     C                   EVAL      VABLNP = 255
036000081204     C                   EVAL      VATLNP = 255
036100081203     C                   EVAL      VABCTR = 000
036200070730     C                   EVAL      VABCBO = '1'
036300081203     C                   MOVEL(P)  '7Q'          VABCTM
036400070530     C                   MOVEL     '1'           VABCBO
036500070222     C* ... e poi verifico se sono stati passati come parametri
036600070222     C                   IF        vlrppt > *blanks
036700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036900070222     C                   EXSR      CHKNUM
037000070222     C                   IF        PiInt=*on
037100070222     C                   Z-ADD     PiVal         VABCCM
037200070222     C                   Z-ADD     PiVal         VATCCM
037300070222     C                   ENDIF
037400070222     C                   ENDIF
037500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037700070222     C                   EXSR      CHKNUM
037800070222     C                   IF        PiInt=*on
037900070222     C                   Z-ADD     PiVal         VABLNP
038000070222     C                   Z-ADD     PiVal         VATLNP
038100070222     C                   ENDIF
038200070222     C                   ENDIF
038300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038500070222     C                   EXSR      CHKNUM
038600070222     C                   IF        PiInt=*on
038700070222     C                   Z-ADD     PiVal         VABCTR
038800070222     C                   ENDIF
038900070222     C                   ENDIF
039000070222     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300000801     C*----------------------------------------------------*
039400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039500000801     C*----------------------------------------------------*
039600010201     C     IMPVAB        BEGSR
039700070530     C*
039800070530     C                   SETOFF                                       3132
039900021113     C*
040000021113     C                   MOVE(P)   vlrpoi        VABFGS
040100021113     C                   MOVE(P)   vlrpoi        VATFGS
040200020725     C*
040300070530     C* Campi anno e mese/giorno
040400070530     C                   MOVEL     datcor        VABAAS
040500070530     C                   MOVEL     datcor        VATAAS
040600070530     C                   MOVE      datcor        VABMGS
040700070730     C*
040800070730     C* Reperimento campi ALFA
040900070730     C*
041000070730     C* Considerazioni sulla ragione sociale del destinatario da indicare
041100081203     C                   EVAL      VABRSD=%trim(%subst(vindta:25:30))
041200070730     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041300070730     C     '@':'A'       XLATE     VABRSD        VABRSD
041400070730     C* ==
041500081203     C                   EVAL      VABIND=%trim(%subst(vindta:55:30))
041600081203     C                   EVAL      VABLOD=%trim(%subst(vindta:90:20))
041700081203     C                   EVAL      VABPRD=%trim(%subst(vindta:110:2))
041800081203     C                   EVAL      VABRMA=%trim(%subst(vindta:114:8))
041900081203     C                   IF        %subst(vindta:140:1) = 'X'
042000081203     C                   EVAL      FlgCAS = '1'
042100081203     C                   ENDIF
042200070730     C*
042300070730     C* Reperimento campi NUMERICI
042400070730     C* NSP/RMN
042500081203     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
042600070730     C                   EXSR      CHKNUM
042700070730     C                   IF        PiInt=*on
042800070730     C                   Z-ADD     PiVal         VABNSP
042900070730     C                   Z-ADD     PiVal         VATNSP
043000070730     C                   Z-ADD     PiVal         VABRMN
043100070730     C                   ELSE
043200070730     C                   SETON                                        31
043300070730     C                   Z-ADD     *zeros        VABNSP
043400070730     C                   Z-ADD     *zeros        VATNSP
043500070730     C                   Z-ADD     1             VABRMN
043600070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043700070730     C                             + ' ' + 'VABNSP VATNSP VABRMN'
043800070730     C                   ENDIF
043900070730     C* CAD
044000081203     C                   EVAL      PiStr=%trim(%subst(vindta:85:5))
044100070730     C                   EXSR      CHKNUM
044200070730     C                   IF        PiInt=*on
044300070730     C                   Z-ADD     PiVal         Num5_0
044400070730     C                   MOVEL(p)  Num5_0        VABCAD
044500070730     C                   ELSE
044600070730     C                   SETON                                        32
044700070730     C                   EVAL      VABCAD = *zeros
044800070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044900070730     C                             + ' ' + 'VABCAD'
045000070730     C                   ENDIF
045100070730     C* Reperisco la provincia dal CAP e dalla località
045200070730     C                   IF        VABCAD <> *blanks AND
045300070730     C                             VABPRD  = *blanks
045400070730     C                   CLEAR                   TISI95DS
045500070730     C                   EVAL      I95TCN = '3'
045600070730     C                   Z-ADD     datcor        I95DAT
045700070730     C                   EVAL      I95CAP = VABCAD
045800070730     C                   EVAL      I95LOC = VABLOD
045900070730     C                   EVAL      I95NAR = VABNZD
046000070730     C                   CALL      'TISI95R'
046100070730     C                   PARM                    TISI95DS
046200070730     C                   EVAL      VABPRD = O95PRV
046300070730     C                   ENDIF
046400070730     C* NCL
046500081203     C                   EVAL      PiStr=%trim(%subst(vindta:16:3))
046600070730     C                   EXSR      CHKNUM
046700070730     C                   IF        PiInt=*on
046800081203     C                   Z-ADD     PiVal         VABNCL
046900070730     C                   ELSE
047000070730     C                   SETON                                        32
047100070730     C                   Z-ADD     *zeros        VABNCL
047200070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047300070730     C                             + ' ' + 'VABNCL'
047400070730     C                   ENDIF
047500070730     C* PKB
047600081203     C                   EVAL      PiStr=%trim(%subst(vindta:19:6))
047700070730     C                   EVAL      PiDecChr = '.'
047800070730     C                   EXSR      CHKNUM
047900070730     C                   IF        PiNum=*on
048000081203     C                   ADD       PiVal         VABPKB
048100070730     C                   ELSE
048200070730     C                   SETON                                        32
048300070730     C                   Z-ADD     *zeros        VABPKB
048400070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048500070730     C                             + ' ' + 'VABPKB'
048600070730     C                   ENDIF
048700070730     C*
048800070730     C* Considerazioni finali su CBO/CAS
048900070730     C                   IF        FlgCAS = '1'
049000070730     C                   IF        VABCBO = '1'
049100070730     C                   EVAL      VABCBO = '4'
049200070730     C                   ENDIF
049300070730     C                   IF        VABCBO = '2'
049400070730     C                   EVAL      VABCBO = '6'
049500070730     C                   ENDIF
049600070730     C                   ENDIF
049700020305     C*
049800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049900011113     C                   EXSR      CHKIMPDIV
050000010202     C*
050100000801     C* Ebbene...
050200000801     C                   ADD       1             §CTRMO
050300070530     C                   IF        *in31 <> *zeros OR
050400070530     C                             *in32 <> *zeros
050500000801     C                   ADD       1             §CTRNO
050600000801     C                   EVAL      recko = vindta
050700000801     C                   EXCEPT    rigadett
050800020725     C                   EVAL      x_vinflg = '2'
050900000801     C                   ELSE
051000010201     C                   ADD       1             §CTROKVB
051100000801     C                   ENDIF
051200000801     C*
051300000801     C                   ENDSR
051400010201     C*----------------------------------------------------*
051500021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051600010201     C*----------------------------------------------------*
051700081203     C     WRIVATE       BEGSR
051800081203     C*
051900081204     C                   z-add     *zeros        wLenWhoAre        2 0
052000081204     C*
052100081204     C                   eval      VATNOT=%trim(%subst(vindta:141:10))
052200081203     C                   if        VATNOT <> *blanks
052300081204     C                   eval      wLenWhoAre = %len(%trim(VATNOT))
052400081204     C*
052500081204     C* Aggiungo costante prima del chi-sono x conformità etichetta
052600081204     C                   select
052700081204     C                   when      wLenWhoAre = 5
052800081204     C                   eval      VATNOT = 'M000' + %trim(VATNOT)
052900081204     C                   when      wLenWhoAre = 6
053000081204     C                   eval      VATNOT = 'M00' + %trim(VATNOT)
053100081204     C                   when      wLenWhoAre = 7
053200081204     C                   eval      VATNOT = 'M0' + %trim(VATNOT)
053300081204     C                   when      wLenWhoAre = 8
053400081204     C                   eval      VATNOT = 'M' + %trim(VATNOT)
053500081204     C                   other
053600081204     C                   endsl
053700081204     C*
053800081203     C                   eval      VATTRC = 'E'
053900081203     C                   write     FIVAT000
054000081203     C                   endif
054100010201     C*
054200010201     C                   ENDSR
054300020904
054400020904
054500010202     C*----------------------------------------------------*
054600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054700010202     C*----------------------------------------------------*
054800020305     C     PREVAT        BEGSR
054900010202     C*
055000021113     C* Compongo il nome del membro da dare al FIVATWWR
055100010202     C                   eval      parmbr = vlrhdl
055200010202     C                   movel     'M'           parmbr
055300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
055400010202     C                   eval      paropz = '1'
055500010202     C* Effettuo la chiamata al CLLE preposto
055600050128     C                   call(e)   'TITVVTC'
055700010202     C                   parm                    parccm
055800010202     C                   parm                    parmbr
055900010202     C                   parm                    paropz
056000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056100010202     C                   if        %error
056200010202     C                   movel     '1'           chkcall
056300010202     C                   else
056400010202     C                   movel     '0'           chkcall
056500010202     C                   endif
056600010202     C*
056700010202     C                   ENDSR
056800081203
056900081203
057000081203
057100081203     C*----------------------------------------------------*
057200081203     C*  CONTROLLO NUMERICITA' CAMPI
057300081203     C*----------------------------------------------------*
057400081203     C     CHKNUM        BEGSR
057500081203     C*
057600081203     C                   IF        PiDecChr = *blanks
057700081203     C                   EVAL      PiDecChr = '.'
057800081203     C                   ENDIF
057900081203     C*
058000081203     C                   callp(e)  UBISNUM_Check(PiStr
058100081203     C                                          :PiDecChr
058200081203     C                                          :PiVal
058300081203     C                                          :PiNum
058400081203     C                                          :PiInt)
058500081203     C*
058600081203     C                   IF        %error
058700081203     C                   EVAL      PiInt=*off
058800081203     C                   ENDIF
058900081203     C*
059000081203     C                   ENDSR
059100081203     C***
059200081203
059300081203
059400000801
059500011113
059600011113     C*----------------------------------------------------*
059700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059800011113     C*----------------------------------------------------*
059900011113     C     CHKIMPDIV     BEGSR
060000011113     C*
060100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060200011113     C                   Z-ADD     *zeros        wrkDec            9 9
060300011113     C*
060400011113     C* Come prima cosa effettuo considerazioni sulla divisa
060500011113     C                   IF        vabIAS > *zeros
060600011113     C                   IF        vabVAS <> 'EUR'
060700011113     C                   EVAL      vabVAS =  'ITL'
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C                   IF        vabCAS > *zeros
061200011113     C                   IF        vabVCA <> 'EUR'
061300011113     C                   EVAL      vabVCA =  'ITL'
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C                   IF        vabVMD > *zeros
061800020305     C                   IF        vabVAD <> 'EUR'
061900011113     C                   EVAL      vabVAD =  'ITL'
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062400011113     C                   Z-ADD     vabIAS        wrkDec
062500011113     C                   IF        wrkDec > *zeros
062600011113     C                   IF        vabVAS = 'ITL'
062700011113     C                   EVAL      vabIAS = *zeros
062800011113     C                   ENDIF
062900011113     C                   ENDIF
063000011113     C*
063100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063200011113     C                   Z-ADD     vabCAS        wrkDec
063300011113     C                   IF        wrkDec > *zeros
063400011113     C                   IF        vabVCA = 'ITL'
063500011113     C                   EVAL      vabCAS = *zeros
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064000011113     C                   Z-ADD     vabVMD        wrkDec
064100011113     C                   IF        wrkDec > *zeros
064200011113     C                   IF        vabVAD = 'ITL'
064300011113     C                   EVAL      vabVMD = *zeros
064400011113     C                   ENDIF
064500011113     C                   ENDIF
064600011113     C*
064700011113     C                   ENDSR
064800011113     C***
064900011113
065000011113
065100000801
065200000801
065300990920      /TITLE Invio dei dati al punto operativo.
065400010202     C     invio         BEGSR
065500990920     C*
065600021113     C* 1° invio FIVAT
065700010201     C                   reset                   dscmz
065800010201     C                   move      vlrpoi        cmzdst
065900021113     C                   eval      cmzfld = 'FIVATWWR'
066000010201     C                   eval      cmzmbd = vlrhdl
066100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066200021009     C***                if        prmfir = *blanks
066300021113     C                   eval      cmzfla = 'FIVAT00F'
066400021113     C                   eval      cmzmba = 'FIVAT00F'
066500021009     C***                else
066600021009     C***                eval      cmzfla = prmfir
066700021009     C***                eval      cmzmba = prmfir
066800021009     C***                endif
066900010201     C                   eval      cmznrr = *zeros
067000020305     C                   move      §ctrokvt      cmznrr
067100021018     C                   eval      cmzlba = vlrfl1
067200010201     C                   call(e)   'TIS711C'
067300010201     C                   parm                    dscmz
067400010201     C                   parm      *blanks       esito
067500010205     C                   if        %error
067600010205     C                             or cmzerr = '1'
067700010205     C                             or esito  = '1'
067800010205     C                   eval      wrkesito = '3'
067900010205     C                   else
068000010201     C*
068100021113     C* 2° invio FIVAB
068200010201     C                   reset                   dscmz
068300010201     C                   move      vlrpoi        cmzdst
068400010201     C                   eval      cmzfld = vlrfou
068500010201     C                   eval      cmzmbd = vlrhdl
068600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068700021009     C***                if        prmfir = *blanks
068800021113     C                   eval      cmzfla = 'FIVAB00F'
068900021113     C                   eval      cmzmba = 'FIVAB00F'
069000021009     C***                else
069100021009     C***                eval      cmzfla = prmfir
069200021009     C***                eval      cmzmba = prmfir
069300021009     C***                endif
069400010201     C                   eval      cmznrr = *zeros
069500010201     C                   move      §ctrokvb      cmznrr
069600021018     C                   eval      cmzlba = vlrfl1
069700010201     C                   call(e)   'TIS711C'
069800010201     C                   parm                    dscmz
069900010201     C                   parm      *blanks       esito
070000010201     C                   if        %error
070100010201     C                             or cmzerr = '1'
070200010201     C                             or esito  = '1'
070300010201     C                   eval      wrkesito = '3'
070400010201     C                   endif
070500010205     C                   endif
070600990920     C*
070700000613     C                   ENDSR
070800000613     C***
070900070411
071000070411     C     *pssr         BEGSR
071100070411     C*
071200070411     C                   if        %open(tivin00r)
071300070411     C                   close     tivin00r
071400070411     C                   endif
071500070411     C                   if        %open(fivabwwr)
071600070411     C                   close     fivabwwr
071700070411     C                   endif
071800070411     C                   if        %open(fivatwwr)
071900070411     C                   close     fivatwwr
072000070411     C                   endif
072100070411     C*
072200070411     C* Effettuo la chiamata al CLLE preposto
072300070411     C                   call(e)   'TITVVTC'
072400070411     C                   parm                    parccm
072500070411     C                   parm                    parmbr
072600070411     C                   parm      '2'           paropz
072700070411     C*
072800070411     C                   eval      wrkesito = '2'
072900070411     C*
073000070411     C                   seton                                        LR
073100070411     C*
073200070411     C                   ENDSR     '*CANCL'
073300070411     C***
073400070411
073500990910
073600000613     C     *inzsr        BEGSR
073700990910     C*
073800990910     C     *entry        plist
073900990920     C                   parm                    tivlrds
074000990921     C                   parm      wrkesito      esito
074100000724     C                   parm                    prmlit
074200000710     C                   parm                    prmfir
074300000613     C*
074400000830     C* CALCOLA LA DATA CORRENTE
074500000830     C                   time                    wn14             14 0
074600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074700000830     C                   z-add     wn8           g08dat
074800000830     C                   z-add     *zeros        g08inv
074900000830     C                   movel     '0'           g08err
075000000830     C                   call      'XSRDA8'
075100000830     C                   parm                    wlbda8
075200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075300000830     C*
075400000613     C                   ENDSR
075500000613     C***
075600990908
075700081203     Otitv1k7p  E            riepilogo         2
075800990915     O                                              'Upload via Internet'
075900990915     O                                           +1 'Traduzione TIVIN00R -'
076000021113     O                                           +1 'FIVABWWR/FIVATWWR'
076100010201     O                                           +1 'Report testate bolle'
076200990915     O          E            riepilogo   2
076300990915     O                       wrkdata
076400990915     O                       wrkora              +1
076500990915     O                       procname            +1
076600990915     O          E            riepilogo   2
076700990915     O                                              'Cliente..................:'
076800990915     O                       VABCCM        z     +1
076900990915     O          E            riepilogo   2
077000990920     O                                              'Riferimento Strategi.....:'
077100990920     O                       vlrhdl              +1
077200990915     O          E            riepilogo   2
077300990915     O                                              'Giusti...................:'
077400010201     O                       §CTROKVB      2   +  1
077500990915     O          E            riepilogo   2
077600990915     O                                              'Sbagliati e corretti.....:'
077700971022     O                       §CTRMO        2   +  1
077800990915     O          E            riepilogo   2
077900990915     O                                              'Sbagliati e scartati.....:'
078000971022     O                       §CTRNO        2   +  1
078100000613
078200081203     Otitv1k7ps E            testdett          2
078300000613     O                                              'Upload via Internet'
078400000613     O                                           +1 'Traduzione TIVIN00R -'
078500021113     O                                           +1 'FIVABWWR/FIVATWWR'
078600010201     O                                           +1 'Report testate bolle'
078700000616     O          E            testdett    3
078800000613     O                                           +2 'N° rec'
078900000613     O                                           +3 'Anteprima contenuto'
079000000616     O          E            rigadett    2
079100000613     O                       rrnum               +2
079200000621     O                       recko               +3
079300000616     O          E            findett     2
079400000613     O                       wrkdata
079500000613     O                       wrkora              +1
079600000613     O                       procname            +1
079700000616     O          E            findett     2
079800000613     O                                              'Cliente..................:'
079900000613     O                       VABCCM        z     +1
080000000616     O          E            findett     2
080100000613     O                                              'Riferimento Strategi.....:'
080200000613     O                       vlrhdl              +1
080300000616     O          E            findett     2
080400000613     O                                              'Giusti...................:'
080500010201     O                       §CTROKVB      2   +  1
080600000616     O          E            findett     2
080700000613     O                                              'Sbagliati e corretti.....:'
080800000613     O                       §CTRMO        2   +  1
080900000616     O          E            findett     2
081000000613     O                                              'Sbagliati e scartati.....:'
081100000613     O                       §CTRNO        2   +  1
081200000512** CMD - COMANDI CL
081300081203OVRPRTF FILE(TITV1K7P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081400081203OVRPRTF FILE(TITV1K7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081500081203DLTOVR FILE(TITV1K7P TITV1K7PS) LVL(*)
081600000512
081700000512
