000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800051209     Ftitvp6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100051209     Ftitvp6ps  O    f  198        PRINTER usropn
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
002600051214     D trul13ds      e ds                  inz
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600051212     D depdest         s             15    INZ(*blanks)
003700051212     D deprif          s             10    INZ(*blanks)
003800051209     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300051209     D wRIF            s              7    INZ(*blanks)
004400051214     d wdcr            s              8  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400000830     D*
005500020725
005600020725     D*------------------
005700051209     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005800020725     D*------------------
005900051209     D RECORDSETDS     DS                  INZ
006000051209     D  VINDTA                     2048
006100051209     D  VINFLG                        1
006200051212     D  VINDEST                      15
006300051212     D  VINRIF                       10
006400051209     D  VINRRN                        8  0
006500060109     D*------------------
006600060109     D* DS REPERIMENTO NUMERATORE
006700060109     D*------------------
006800060109     D trul33ds      e ds                  inz
006900060109     D*------------------
007000060109     D* DS ARCHITETTURA
007100060109     D*------------------
007200060109     D kpjba         e ds                  inz
007300060109     D*------------------
007400020725     D*
007500990908
007600010201
007700010201
007800990915     C                   time                    wrkdata
007900990915     C                   time                    wrkora
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000724     C*
008400000724     C* SE OCCORRE SPEDIRE IN FILIALE
008500000724     C                   if        vlrpoi <> *zeros
008600000724     C*
008700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000724     C     vlrpoi        chain     azorg01l
009000000724     C                   if        %found
009100000616     C                   movel(p)  CMD(1)        depcmd
009200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000724     C*
010000000724     C                   endif
010100000724     C                   endif
010200000616     C*
010300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400000616     C                   movel(p)  CMD(2)        depcmd
010500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010600000616     C*
010700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000616     C                   PARM                    depcmd
011100000616     C                   PARM                    LENGH
011200000616     C*
011300051209     C                   if        not %open(titvp6ps)
011400051209     C                   open      titvp6ps
011500000616     C                   except    testdett
011600000613     C                   endif
011700000613     C*
011800051209     C                   EXSR      RWVAB                                        LETT/SCR. VAB
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100010202     C* Effettuo la chiamata al CLLE preposto
012200050128     C                   call(e)   'TITVVTC'
012300010202     C                   parm                    parccm
012400010202     C                   parm                    parmbr
012500010202     C                   parm      '2'           paropz
012600010202     C*
012700051209     C                   if        %open(titvp6ps)
012800000616     C                   except    findett
012900051209     C                   close     titvp6ps
013000000613     C                   endif
013100000616     C*
013200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    CMD(3)
013600000616     C                   PARM                    LENGH
013700000616     C*
013800000801     C
013900010201     C                   seton                                        LR
014000000613
014100000613     C*--------------------------------------------------------
014200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014300000613     C*--------------------------------------------------------
014400000621     C     STPR          BEGSR
014500000613     C*
014600051209     C                   if        not %open(titvp6p)
014700051209     C                   open      titvp6p
014800990915     C                   endif
014900990915     C*
015000990915     C                   except    riepilogo
015100990915     C*
015200051209     C                   if        %open(titvp6p)
015300051209     C                   close     titvp6p
015400990914     C                   endif
015500990910     C*
015600000613     C                   ENDSR
015700000613     C***
015800990908
015900000801
016000910830     C*--------------------------------------------------------
016100051209     C* RWVAB    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016200910830     C*--------------------------------------------------------
016300051209     C     RWVAB         BEGSR
016400990910     C*
016500990914     C                   if        not %open(tivin00r)
016600990908     C                   open      tivin00r
016700990914     C                   endif
016800021113     C                   if        not %open(fivabwwr)
016900021113     C                   open      fivabwwr
017000990914     C                   endif
017100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017200020305     C                   exsr      prevat
017300010201     C*
017400010202     C                   if        chkcall = '0'
017500010202     C*
017600021113     C                   if        not %open(fivatwwr)
017700021113     C                   open      fivatwwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   clear                   §CTROKVB          5 0
018100020305     C                   clear                   §CTROKVT          5 0
018200000801     C                   clear                   §CTRMO            5 0
018300000801     C                   clear                   §CTRNO            5 0
018400990910     C*
018500020725     C*
018600020725     C/EXEC SQL
018700020725     C+ declare C1 cursor for select
018800051212     C***+ vindta, vinflg, substr(vindta, 203, 92) as dest,
018900051212     C+ vindta, vinflg, substr(vindta, 431, 15) as dest,
019000051212     C+ substr(vindta, 446, 10) as rif,
019100051209     C+ rrn(tivin00r)
019200051209     C+ from tivin00r order by dest, rif
019300020725     C+ for read only
019400020725     C/END-EXEC
019500020725     C
019600020725     C/EXEC SQL
019700020725     C+ open C1
019800020725     C/END-EXEC
019900020725     C
020000020725     C/EXEC SQL
020100051209     C+ Fetch C1 into :RECORDSETDS
020200020725     C/END-EXEC
020300020725     C*
020400020725     C                   dow       sqlcod = *zeros
020500990913     C*
020600020725     C                   if        vindta > *blanks
020700000613     C                   add       1             rrnum
020800000801     C*
020900020725     C                   if        vinflg = *blanks
021000020725     C                             or vinflg = '0'
021100020725     C                             or vinflg = '2'
021200000801     C*
021300020725     C                   clear                   x_vinmsg
021400020725     C                   eval      x_vinflg = '1'
021500010305     C*
021600051209     C* A rottura d destinatario eseguo operazioni
021700051209     C                   if        depdest = *blanks                            => 1° giro
021800051209     C                   eval      depdest = VINDEST
021900051209     C                   eval      deprif  = VINRIF
022000020725     C                   clear                   tivinds
022100020725     C                   exsr      inzvar
022200020725     C                   exsr      defcam
022300020305     C                   exsr      impvab
022400051209     C                   exsr      addvab
022500010305     C                   else
022600051209     C                   if        depdest <> VINDEST                           => rottura
022700051209     C                   eval      depdest  = VINDEST                           => memorizz. new rot
022800051209     C                   eval      deprif  = VINRIF
022900020305     C                   exsr      wrivab
023000020725     C                   clear                   tivinds
023100020725     C                   exsr      inzvar
023200020725     C                   exsr      defcam
023300020305     C                   exsr      impvab
023400051209     C                   exsr      addvab
023500051209     C                   else                                                   => x stesso dest.
023600051209     C                   if        deprif  <> VINRIF                            => rottura
023700051209     C                   eval      deprif   = VINRIF                            => memorizz. new rot
023800051209     C                   exsr      addvab
023900051209     C                   endif
024000010305     C                   endif
024100010305     C                   endif
024200051209     C                   endif
024300051209     C*
024400051209     C* Eseguo routine x reperimento e scrittura "chi sono"
024500051209     C                   EXSR      WRIVAT
024600000905     C*
024700000905     C                   else
024800020725     C                   eval      x_vinflg = '1'
024900000905     C                   endif
025000000905     C*
025100020725     C     VINRRN        chain     tivin000
025200020725     C                   if        %found(tivin00r)
025300020725     C                   eval      y_vinflg = x_vinflg
025400020725     C                   eval      y_vinmsg = x_vinmsg
025500020725     C                   update    tivin000
025600020725     C                   endif
025700020725     C*
025800020725     C/EXEC SQL
025900051209     C+ Fetch C1 into :RECORDSETDS
026000020725     C/END-EXEC
026100020725     C*
026200020725     C                   enddo
026300020725     C*
026400020725     C/EXEC SQL
026500020725     C+ close C1
026600020725     C/END-EXEC
026700000905     C*
026800020305     C* Scarico i VAB rimasti "in sospeso"
026900020305     C                   exsr      wrivab
027000010202     C*
027100010202     C                   endif
027200990910
027300990910     C* Se non ci sono record con errori ...
027400000710     C                   if        §ctrno = 0
027500990910     C* ... restituisco esito OK.
027600990921     C                   eval      wrkesito = '0'
027700990910     C                   else
027800010201     C                   if        §ctrokvb > 0
027900990921     C                   eval      wrkesito = '1'
028000000710     C                   else
028100000710     C                   eval      wrkesito = '2'
028200990910     C                   endif
028300000710     C                   endif
028400990910     C*
028500990914     C                   if        %open(tivin00r)
028600990908     C                   close     tivin00r
028700990914     C                   endif
028800021113     C                   if        %open(fivabwwr)
028900021113     C                   close     fivabwwr
029000990914     C                   endif
029100021113     C                   if        %open(fivatwwr)
029200021113     C                   close     fivatwwr
029300010201     C                   endif
029400990910     C*
029500010201     C                   if        §ctrokvb > 0
029600000724     C                             and vlrpoi <> *zeros
029700010202     C                   exsr      invio
029800990920     C                   endif
029900990920     C*
030000910830     C                   ENDSR
030100000613     C***
030200010305
030300010305     C*----------------------------------------------------*
030400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030500010305     C*----------------------------------------------------*
030600051222     C     WRIVAB        BEGSR
030700051222     C*
030800051222     C* Considerazioni sul contenuto di campi precedentemente valorizzati
030900051222     C                   IF        FlgCAS <> '0'
031000051222     C                   IF        VABCBO = '1'
031100051222     C                   EVAL      VABCBO = '4'
031200051222     C                   ELSE
031300051222     C                   EVAL      VABCBO = '6'
031400051222     C                   ENDIF
031500051222     C                   ENDIF
031600051222     C*
031700051222     C* Eseguo routine finale x considerazioni specifiche su importi/divise
031800051222     C                   EXSR      CHKIMPDIV
031900051222     C*
032000051222     C* Ebbene...
032100051222     C                   ADD       1             §CTRMO
032200051222     C                   IF        errore <> *zeros
032300051222     C                   ADD       1             §CTRNO
032400051222     C                   EVAL      recko = vindta
032500051222     C                   EXCEPT    rigadett
032600051222     C                   EVAL      x_vinflg = '2'
032700051222     C                   ELSE
032800051222     C                   ADD       1             §CTROKVB
032900051222     C                   ENDIF
033000010305     C*
033100021113     C                   write     fivab000                                     => scarico il VAB
033200010305     C*
033300010305     C                   ENDSR
033400990920
033500000801     C*----------------------------------------------------*
033600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033700000801     C*----------------------------------------------------*
033800010201     C     INZVAR        BEGSR
033900000801     C*
034000051209     C                   Z-ADD     *zeros        Num5_0            5 0
034100051209     C                   Z-ADD     *zeros        wGiro             8 0
034200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034300020725     C                   MOVEL     '0'           FlgCAS            1
034400000801     C*
034500000801     C                   ENDSR
034600000801     C*----------------------------------------------------*
034700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034800000801     C*----------------------------------------------------*
034900020904     C     DEFCAM        BEGSR
035000000801     C*
035100021113     C                   CLEAR                   FIVAB000
035200021113     C                   CLEAR                   FIVAT000
035300051209     C                   Z-ADD     0012818       VABCCM
035400051209     C                   Z-ADD     0012818       VATCCM
035500020725     C                   Z-ADD     001           VABLNP
035600020725     C                   Z-ADD     001           VATLNP
035700010201     C                   Z-ADD     000           VABCTR
035800020305     C                   MOVEL     '7Q'          VABCTM
035900051209     C                   MOVEL     'C'           VABTSP
036000051209     C                   MOVEL     '1'           VABCBO
036100020305     C                   MOVEL     'E'           VATTRC
036200051212     C* ... e poi verifico se sono stati passati come parametri
036300051212     C                   IF        vlrppt > *blanks
036400051212     C                   IF        %subst(vlrppt:1:7) <> *blanks
036500051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036600051212     C                   EXSR      CHKNUM
036700051212     C                   IF        PiInt=*on
036800051212     C                   Z-ADD     PiVal         VABCCM
036900051212     C                   Z-ADD     PiVal         VATCCM
037000051212     C                   ENDIF
037100051212     C                   ENDIF
037200051212     C                   IF        %subst(vlrppt:8:3) <> *blanks
037300051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037400051212     C                   EXSR      CHKNUM
037500051212     C                   IF        PiInt=*on
037600051212     C                   Z-ADD     PiVal         VABLNP
037700051212     C                   Z-ADD     PiVal         VATLNP
037800051212     C                   ENDIF
037900051212     C                   ENDIF
038000051212     C                   IF        %subst(vlrppt:11:3) <> *blanks
038100051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038200051212     C                   EXSR      CHKNUM
038300051212     C                   IF        PiInt=*on
038400051212     C                   Z-ADD     PiVal         VABCTR
038500051212     C                   ENDIF
038600051212     C                   ENDIF
038700051212     C                   IF        %subst(vlrppt:14:2) <> *blanks
038800051212     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
038900051212     C                   ENDIF
039000051212     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300000801     C*----------------------------------------------------*
039400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039500000801     C*----------------------------------------------------*
039600010201     C     IMPVAB        BEGSR
039700010305     C*
039800000801     C                   Z-ADD     *zeros        errore            1 0
039900021113     C*
040000021113     C                   MOVE(P)   vlrpoi        VABFGS
040100021113     C                   MOVE(P)   vlrpoi        VATFGS
040200051209     C*
040300051209     C* Valorizzo i campi data spedizione
040400051209     C                   MOVEL     datcor        VABAAS
040500051209     C                   MOVEL     datcor        VATAAS
040600051209     C                   MOVE      datcor        VABMGS
040700020725     C*
040800020725     C* Campi relativi al destinatario
040900051209     C                   EVAL      VABRSD=%trim(%subst(vindta:203:30))
041000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
041200020117     C* ==
041300051209     C                   EVAL      VABIND=%trim(%subst(vindta:233:25))
041400051209     C                   EVAL      VABLOD=%trim(%subst(vindta:258:30))
041500051209     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
041600051212     C                   EVAL      VABRMA=%trim(%subst(vindta:446:10))
041700051209     C                   IF        %subst(vindta:1130:1) = 'E'
041800051209     C                   EVAL      VABTSP='E'
041900051209     C                   ENDIF
042000051209     C                   IF        %subst(vindta:46:2) = '02'
042100051209     C                   EVAL      VABCBO='2'
042200051209     C                   ENDIF
042300051209     C                   MOVEL     *zeros        wAlfaDCR          8
042400051219     C                   EVAL      wAlfaDCR=%trim(%subst(vindta:1095:8))
042500051209     C                   MOVE(P)   wAlfaDCR      VABDCR
042600051214     C                   IF        VABDCR = *zeros
042700051214     C                   EVAL      VABDCR = wdcr
042800051214     C                   ENDIF
042900020725     C*
043000020725     C* Campi numerici
043100051209     C*
043200060109     C* ...VABNSP/VATNSP => Stacco un numeratore da AZNUM
043300060109     C                   clear                   TRUL33DS
043400060109     C                   eval      I33OPE = *zeros
043500060109     C                   eval      I33CNU = 302
043600060109     C                   eval      I33NUM = 1
043700060109     C                   movel     TRUL33DS      KPJBU
043800060109     C                   call      'TRUL33R'
043900060109     C                   parm                    KPJBA
044000060109     C                   movel     KPJBU         TRUL33DS
044100060109     C                   if        O33ERR = *zeros
044200060109     C                   z-add     O33NRF        VABNSP
044300060109     C                   z-add     O33NRF        VATNSP
044400060109     C                   else
044500060109     C                   Z-ADD     1             errore
044600060109     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700060109     C                             + ' ' + 'VABNSP VATNSP'
044800060109     C                   endif
044900060109     C* ...VABRMN
045000051212     C                   EVAL      PiStr=%trim(%subst(vindta:446:10))
045100051209     C                   EXSR      CHKNUM
045200051209     C                   IF        PiInt=*on
045300051209     C                   Z-ADD     PiVal         VABRMN
045400051209     C                   ELSE
045500051209     C                   ADD       1             errore
045600051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045700060109     C                             + ' ' + 'VABRMN'
045800051209     C                   ENDIF
045900051209     C* ...VABCAD
046000051209     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
046100000801     C                   EXSR      CHKNUM
046200000801     C                   IF        PiInt=*on
046300000801     C                   Z-ADD     PiVal         Num5_0
046400010208     C                   MOVEL(p)  Num5_0        VABCAD
046500000801     C                   ELSE
046600000801     C                   ADD       1             errore
046700020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800000801     C                             + ' ' + 'VABCAD'
046900000801     C                   ENDIF
047000000801     C*
047100000801     C                   ENDSR
047200051209     C*----------------------------------------------------*
047300051209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
047400051209     C*----------------------------------------------------*
047500051209     C     ADDVAB        BEGSR
047600051209     C*
047700051209     C* Incremento il contatore dei record elaborati x medesimo destinatario
047800051209     C                   ADD       1             wGiro
047900051209     C* ...VABNOT/VABNT2
048000051209     C                   IF        wGiro > 1
048100051212     C                   EVAL      wRIF = %trim(%subst(vindta:446:10))
048200051209     C                   IF        %len(%trim(VABNOT)) <=
048300051209     C                             %size(VABNOT) - %size(wRIF)
048400051209     C                   EVAL      VABNOT = %trim(VABNOT) + '-' + wRIF
048500051209     C                   ELSE
048600051209     C                   EVAL      VABNT2 = %trim(VABNT2) + '-' + wRIF
048700051209     C                   ENDIF
048800051209     C                   ENDIF
048900051209     C* ...VABNCL
049000051209     C                   EVAL      PiStr=%trim(%subst(vindta:335:4))
049100051209     C                   EXSR      CHKNUM
049200051209     C                   IF        PiInt=*on
049300051209     C                   ADD       PiVal         VABNCL
049400051209     C                   ELSE
049500051209     C                   ADD       1             errore
049600051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700051209     C                             + ' ' + 'VABNCL'
049800051209     C                   ENDIF
049900051209     C* ...VABPKB
050000051209     C                   EVAL      PiStr=%trim(%subst(vindta:371:8))
050100051209     C                   EXSR      CHKNUM
050200051209     C                   IF        PiNum=*on
050300051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
050400051209     C                   ADD(H)    PiVal         VABPKB
050500051209     C                   ELSE
050600051209     C                   ADD       1             errore
050700051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050800051209     C                             + ' ' + 'VABPKB'
050900051209     C                   ENDIF
051000051209     C* ...VABCAS
051100051209     C                   IF        %trim(%subst(vindta:681:10)) <> *zeros
051200051209     C                   EVAL      FlgCAS='1'
051300051209     C                   EVAL      VABVCA='EUR'
051400051209     C                   EVAL      PiStr=%trim(%subst(vindta:681:10))
051500051209     C                   EXSR      CHKNUM
051600051209     C                   IF        PiNum=*on
051700051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
051800051209     C                   ADD(H)    PiVal         VABCAS
051900051209     C                   ELSE
052000051209     C                   ADD       1             errore
052100051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052200051209     C                             + ' ' + 'VABCAS'
052300051209     C                   ENDIF
052400051209     C                   ENDIF
052500051209     C*
052600051209     C                   ENDSR
052700010201     C*----------------------------------------------------*
052800021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
052900010201     C*----------------------------------------------------*
053000051209     C     WRIVAT        BEGSR
053100010201     C*
053200021113     C* Valorizzo l buffer di scrittura del FIVAT
053300051209     C                   eval      VATNOT = %trim(%subst(vindta:1:45))
053400021113     C                   write     FIVAT000
053500010201     C*
053600010201     C                   ENDSR
053700020904
053800020904
053900010202     C*----------------------------------------------------*
054000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054100010202     C*----------------------------------------------------*
054200020305     C     PREVAT        BEGSR
054300010202     C*
054400021113     C* Compongo il nome del membro da dare al FIVATWWR
054500010202     C                   eval      parmbr = vlrhdl
054600010202     C                   movel     'M'           parmbr
054700051209     C                   eval      parccm = vlrksc
054800010202     C                   eval      paropz = '1'
054900010202     C* Effettuo la chiamata al CLLE preposto
055000050128     C                   call(e)   'TITVVTC'
055100010202     C                   parm                    parccm
055200010202     C                   parm                    parmbr
055300010202     C                   parm                    paropz
055400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055500010202     C                   if        %error
055600010202     C                   movel     '1'           chkcall
055700010202     C                   else
055800010202     C                   movel     '0'           chkcall
055900010202     C                   endif
056000010202     C*
056100010202     C                   ENDSR
056200000801     C*----------------------------------------------------*
056300000801     C*  CONTROLLO NUMERICITA' CAMPI
056400000801     C*----------------------------------------------------*
056500000801     C     CHKNUM        BEGSR
056600000801     C*
056700000801     C                   call(e)   'ISNUMERIC'
056800000801     C                   PARM                    PiStr            30
056900000801     C                   PARM      ','           PiDecChr          1
057000000801     C                   PARM      *ZEROS        PiVal            30 9
057100000801     C                   PARM      '0'           PiInt             1
057200000801     C                   PARM      '0'           PiNum             1
057300000801     C                   IF        %error
057400000801     C                   EVAL      PiInt=*off
057500000801     C                   ENDIF
057600000801     C*
057700000801     C                   ENDSR
057800000801     C***
057900000801
058000011113
058100011113     C*----------------------------------------------------*
058200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058300011113     C*----------------------------------------------------*
058400011113     C     CHKIMPDIV     BEGSR
058500011113     C*
058600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058700011113     C                   Z-ADD     *zeros        wrkDec            9 9
058800011113     C*
058900011113     C* Come prima cosa effettuo considerazioni sulla divisa
059000011113     C                   IF        vabIAS > *zeros
059100011113     C                   IF        vabVAS <> 'EUR'
059200011113     C                   EVAL      vabVAS =  'ITL'
059300011113     C                   ENDIF
059400011113     C                   ENDIF
059500011113     C*
059600011113     C                   IF        vabCAS > *zeros
059700011113     C                   IF        vabVCA <> 'EUR'
059800011113     C                   EVAL      vabVCA =  'ITL'
059900011113     C                   ENDIF
060000011113     C                   ENDIF
060100011113     C*
060200011113     C                   IF        vabVMD > *zeros
060300020305     C                   IF        vabVAD <> 'EUR'
060400011113     C                   EVAL      vabVAD =  'ITL'
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060900011113     C                   Z-ADD     vabIAS        wrkDec
061000011113     C                   IF        wrkDec > *zeros
061100011113     C                   IF        vabVAS = 'ITL'
061200011113     C                   EVAL      vabIAS = *zeros
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061700011113     C                   Z-ADD     vabCAS        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVCA = 'ITL'
062000011113     C                   EVAL      vabCAS = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062500011113     C                   Z-ADD     vabVMD        wrkDec
062600011113     C                   IF        wrkDec > *zeros
062700011113     C                   IF        vabVAD = 'ITL'
062800011113     C                   EVAL      vabVMD = *zeros
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C                   ENDSR
063300011113     C***
063400011113
063500011113
063600000801
063700000801
063800990920      /TITLE Invio dei dati al punto operativo.
063900010202     C     invio         BEGSR
064000990920     C*
064100021113     C* 1° invio FIVAT
064200010201     C                   reset                   dscmz
064300010201     C                   move      vlrpoi        cmzdst
064400021113     C                   eval      cmzfld = 'FIVATWWR'
064500010201     C                   eval      cmzmbd = vlrhdl
064600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064700021009     C***                if        prmfir = *blanks
064800021113     C                   eval      cmzfla = 'FIVAT00F'
064900021113     C                   eval      cmzmba = 'FIVAT00F'
065000021009     C***                else
065100021009     C***                eval      cmzfla = prmfir
065200021009     C***                eval      cmzmba = prmfir
065300021009     C***                endif
065400010201     C                   eval      cmznrr = *zeros
065500020305     C                   move      §ctrokvt      cmznrr
065600021018     C                   eval      cmzlba = vlrfl1
065700010201     C                   call(e)   'TIS711C'
065800010201     C                   parm                    dscmz
065900010201     C                   parm      *blanks       esito
066000010205     C                   if        %error
066100010205     C                             or cmzerr = '1'
066200010205     C                             or esito  = '1'
066300010205     C                   eval      wrkesito = '3'
066400010205     C                   else
066500010201     C*
066600021113     C* 2° invio FIVAB
066700010201     C                   reset                   dscmz
066800010201     C                   move      vlrpoi        cmzdst
066900010201     C                   eval      cmzfld = vlrfou
067000010201     C                   eval      cmzmbd = vlrhdl
067100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067200021009     C***                if        prmfir = *blanks
067300021113     C                   eval      cmzfla = 'FIVAB00F'
067400021113     C                   eval      cmzmba = 'FIVAB00F'
067500021009     C***                else
067600021009     C***                eval      cmzfla = prmfir
067700021009     C***                eval      cmzmba = prmfir
067800021009     C***                endif
067900010201     C                   eval      cmznrr = *zeros
068000010201     C                   move      §ctrokvb      cmznrr
068100021018     C                   eval      cmzlba = vlrfl1
068200010201     C                   call(e)   'TIS711C'
068300010201     C                   parm                    dscmz
068400010201     C                   parm      *blanks       esito
068500010201     C                   if        %error
068600010201     C                             or cmzerr = '1'
068700010201     C                             or esito  = '1'
068800010201     C                   eval      wrkesito = '3'
068900010201     C                   endif
069000010205     C                   endif
069100990920     C*
069200000613     C                   ENDSR
069300000613     C***
069400070411
069500070411     C     *pssr         BEGSR
069600070411     C*
069700070411     C                   if        %open(tivin00r)
069800070411     C                   close     tivin00r
069900070411     C                   endif
070000070411     C                   if        %open(fivabwwr)
070100070411     C                   close     fivabwwr
070200070411     C                   endif
070300070411     C                   if        %open(fivatwwr)
070400070411     C                   close     fivatwwr
070500070411     C                   endif
070600070411     C*
070700070411     C* Effettuo la chiamata al CLLE preposto
070800070411     C                   call(e)   'TITVVTC'
070900070411     C                   parm                    parccm
071000070411     C                   parm                    parmbr
071100070411     C                   parm      '2'           paropz
071200070411     C*
071300070411     C                   eval      wrkesito = '2'
071400070411     C*
071500070411     C                   seton                                        LR
071600070411     C*
071700070411     C                   ENDSR     '*CANCL'
071800070411     C***
071900990910
072000000613     C     *inzsr        BEGSR
072100990910     C*
072200990910     C     *entry        plist
072300990920     C                   parm                    tivlrds
072400990921     C                   parm      wrkesito      esito
072500000724     C                   parm                    prmlit
072600000710     C                   parm                    prmfir
072700000613     C*
072800000830     C* CALCOLA LA DATA CORRENTE
072900000830     C                   time                    wn14             14 0
073000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073100000830     C                   z-add     wn8           g08dat
073200000830     C                   z-add     *zeros        g08inv
073300000830     C                   movel     '0'           g08err
073400000830     C                   call      'XSRDA8'
073500000830     C                   parm                    wlbda8
073600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073700051214     C*
073800051214     C* Calcolo oggi + 1 gg lavorativo per la data consegna richiesta qualora nn già
073900051214     C* indicata dal cliente nei record
074000051214     C                   clear                   TRUL13DS
074100051214     C                   eval      I13MOD = 'P'
074200051214     C                   eval      I13FIL = 001
074300051214     C                   eval      I13DIN = datcor
074400051214     C                   movel     wn14          I13HIN
074500051214     C                   eval      I13GIO = 1
074600051214     C                   eval      I13ORE = *zeros
074700051214     C                   call      'TRUL13R'
074800051214     C                   parm                    TRUL13DS
074900051214     C                   if        O13ERR = *blanks
075000051214     C                   eval      wdcr  = O13DFI
075100051214     C                   else
075200051214     C                   eval      wdcr  = datcor
075300051214     C                   endif
075400000830     C*
075500000613     C                   ENDSR
075600000613     C***
075700990908
075800051209     Otitvp6p   E            riepilogo         2
075900990915     O                                              'Upload via Internet'
076000990915     O                                           +1 'Traduzione TIVIN00R -'
076100021113     O                                           +1 'FIVABWWR/FIVATWWR'
076200010201     O                                           +1 'Report testate bolle'
076300990915     O          E            riepilogo   2
076400990915     O                       wrkdata
076500990915     O                       wrkora              +1
076600990915     O                       procname            +1
076700990915     O          E            riepilogo   2
076800990915     O                                              'Cliente..................:'
076900990915     O                       VABCCM        z     +1
077000990915     O          E            riepilogo   2
077100990920     O                                              'Riferimento Strategi.....:'
077200990920     O                       vlrhdl              +1
077300990915     O          E            riepilogo   2
077400990915     O                                              'Giusti...................:'
077500010201     O                       §CTROKVB      2   +  1
077600990915     O          E            riepilogo   2
077700990915     O                                              'Sbagliati e corretti.....:'
077800971022     O                       §CTRMO        2   +  1
077900990915     O          E            riepilogo   2
078000990915     O                                              'Sbagliati e scartati.....:'
078100971022     O                       §CTRNO        2   +  1
078200000613
078300051209     Otitvp6ps  E            testdett          2
078400000613     O                                              'Upload via Internet'
078500000613     O                                           +1 'Traduzione TIVIN00R -'
078600021113     O                                           +1 'FIVABWWR/FIVATWWR'
078700010201     O                                           +1 'Report testate bolle'
078800000616     O          E            testdett    3
078900000613     O                                           +2 'N° rec'
079000000613     O                                           +3 'Anteprima contenuto'
079100000616     O          E            rigadett    2
079200000613     O                       rrnum               +2
079300000621     O                       recko               +3
079400000616     O          E            findett     2
079500000613     O                       wrkdata
079600000613     O                       wrkora              +1
079700000613     O                       procname            +1
079800000616     O          E            findett     2
079900000613     O                                              'Cliente..................:'
080000000613     O                       VABCCM        z     +1
080100000616     O          E            findett     2
080200000613     O                                              'Riferimento Strategi.....:'
080300000613     O                       vlrhdl              +1
080400000616     O          E            findett     2
080500000613     O                                              'Giusti...................:'
080600010201     O                       §CTROKVB      2   +  1
080700000616     O          E            findett     2
080800000613     O                                              'Sbagliati e corretti.....:'
080900000613     O                       §CTRMO        2   +  1
081000000616     O          E            findett     2
081100000613     O                                              'Sbagliati e scartati.....:'
081200000613     O                       §CTRNO        2   +  1
081300000512** CMD - COMANDI CL
081400051209OVRPRTF FILE(TITVP6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
081500051209OVRPRTF FILE(TITVP6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
081600051209DLTOVR FILE(TITVP6P TITVP6PS) LVL(*)
081700000512
081800000512
