000100110215      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600110215     FEDIVABwr  O    E             DISK    usropn
000700110215     FEDIVATwr  O    E             DISK    usropn
000800101122     Ftitv2b4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100101122     Ftitv2b4ps O    f  198        PRINTER usropn
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
002600110216     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600110215     D depspe          s             15    INZ(*blanks)
003700051209     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004200110215     D skSHIP          s             15  0 INZ(*blanks) dim(1000)
004300110215     D skDNS           s             15    INZ(*blanks) dim(1000)
004400110215     D idxDNS          s              4  0 INZ(*zeros)
004500110215     D skWHOIS         s             20    INZ(*blanks) dim(1000)
004600110215     D idxWHOIS        s              4  0 INZ(*zeros)
004700020725
004800020725     D*------------------
004900051209     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005000020725     D*------------------
005100051209     D RECORDSETDS     DS                  INZ
005200051209     D  VINDTA                     2048
005300051209     D  VINFLG                        1
005400110215     D  VINSPED                      15
005500110215     D  VINDNS                       15
005600051209     D  VINRRN                        8  0
005700060109     D*------------------
005800060109     D* DS REPERIMENTO NUMERATORE
005900060109     D*------------------
006000060109     D trul33ds      e ds                  inz
006100060109     D*------------------
006200060109     D* DS ARCHITETTURA
006300060109     D*------------------
006400060109     D kpjba         e ds                  inz
006500060109     D*------------------
006600020725     D*
006700990908
006800010201
006900010201
007000990915     C                   time                    wrkdata
007100990915     C                   time                    wrkora
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500000724     C*
007600000724     C* SE OCCORRE SPEDIRE IN FILIALE
007700000724     C                   if        vlrpoi <> *zeros
007800000724     C*
007900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000724     C     vlrpoi        chain     azorg01l
008200000724     C                   if        %found
008300000616     C                   movel(p)  CMD(1)        depcmd
008400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000724     C*
009200000724     C                   endif
009300000724     C                   endif
009400000616     C*
009500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600000616     C                   movel(p)  CMD(2)        depcmd
009700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009800000616     C*
009900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010000011113     C                   Z-ADD     150           LENGH            15 5
010100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010200000616     C                   PARM                    depcmd
010300000616     C                   PARM                    LENGH
010400000616     C*
010500101122     C                   if        not %open(titv2b4ps)
010600101122     C                   open      titv2b4ps
010700000616     C                   except    testdett
010800000613     C                   endif
010900000613     C*
011000051209     C                   EXSR      RWVAB                                        LETT/SCR. VAB
011100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300010202     C* Effettuo la chiamata al CLLE preposto
011400110215     C                   call(e)   'TITVEVTC'
011500010202     C                   parm                    parccm
011600010202     C                   parm                    parmbr
011700010202     C                   parm      '2'           paropz
011800010202     C*
011900101122     C                   if        %open(titv2b4ps)
012000000616     C                   except    findett
012100101122     C                   close     titv2b4ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900110216     C*
013000110216     C* Effettuo lancio TISI95 solo x chiusura
013100110216     C                   CLEAR                   TISI95DS
013200110216     C                   EVAL      I95TLA = 'C'
013300110216     C                   CALL      'TISI95R'
013400110216     C                   PARM                    TISI95DS
013500000616     C*
013600000801     C
013700010201     C                   seton                                        LR
013800000613
013900000613     C*--------------------------------------------------------
014000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014100000613     C*--------------------------------------------------------
014200000621     C     STPR          BEGSR
014300000613     C*
014400101122     C                   if        not %open(titv2b4p)
014500101122     C                   open      titv2b4p
014600990915     C                   endif
014700990915     C*
014800990915     C                   except    riepilogo
014900990915     C*
015000101122     C                   if        %open(titv2b4p)
015100101122     C                   close     titv2b4p
015200990914     C                   endif
015300990910     C*
015400000613     C                   ENDSR
015500000613     C***
015600990908
015700000801
015800910830     C*--------------------------------------------------------
015900110215     C* RWVAB    LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
016000910830     C*--------------------------------------------------------
016100051209     C     RWVAB         BEGSR
016200990910     C*
016300990914     C                   if        not %open(tivin00r)
016400990908     C                   open      tivin00r
016500990914     C                   endif
016600110215     C                   if        not %open(edivabwr)
016700110215     C                   open      edivabwr
016800990914     C                   endif
016900110215     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
017000020305     C                   exsr      prevat
017100010201     C*
017200010202     C                   if        chkcall = '0'
017300010202     C*
017400110215     C                   if        not %open(edivatwr)
017500110215     C                   open      edivatwr
017600010201     C                   endif
017700990910     C*
017800010201     C                   clear                   §CTROKVB          5 0
017900020305     C                   clear                   §CTROKVT          5 0
018000000801     C                   clear                   §CTRMO            5 0
018100000801     C                   clear                   §CTRNO            5 0
018200990910     C*
018300020725     C*
018400020725     C/EXEC SQL
018500020725     C+ declare C1 cursor for select
018600110215     C+ vindta, vinflg, substr(vindta, 401, 15) as sped,
018700110215     C+ substr(vindta, 431, 15) as dns,
018800051209     C+ rrn(tivin00r)
018900110215     C+ from tivin00r order by sped, dns
019000020725     C+ for read only
019100020725     C/END-EXEC
019200020725     C
019300020725     C/EXEC SQL
019400020725     C+ open C1
019500020725     C/END-EXEC
019600020725     C
019700020725     C/EXEC SQL
019800051209     C+ Fetch C1 into :RECORDSETDS
019900020725     C/END-EXEC
020000020725     C*
020100020725     C                   dow       sqlcod = *zeros
020200990913     C*
020300020725     C                   if        vindta > *blanks
020400000613     C                   add       1             rrnum
020500000801     C*
020600020725     C                   if        vinflg = *blanks
020700020725     C                             or vinflg = '0'
020800020725     C                             or vinflg = '2'
020900000801     C*
021000020725     C                   clear                   x_vinmsg
021100020725     C                   eval      x_vinflg = '1'
021200010305     C*
021300051209     C* A rottura d destinatario eseguo operazioni
021400110215     C                   if        depspe  = *blanks                            => 1° giro
021500110215     C                   eval      depspe  = VINSPED
021600020725     C                   clear                   tivinds
021700020725     C                   exsr      inzvar
021800020725     C                   exsr      defcam
021900020305     C                   exsr      impvab
022000110215     C                   exsr      impdet
022100010305     C                   else
022200110215     C                   if        depspe  <> VINSPED                           => rottura
022300110215     C                   eval      depspe   = VINSPED                           => memorizz. new rot
022400020305     C                   exsr      wrivab
022500020725     C                   clear                   tivinds
022600020725     C                   exsr      inzvar
022700020725     C                   exsr      defcam
022800020305     C                   exsr      impvab
022900110215     C                   exsr      impdet
023000110215     C                   else
023100110215     C                   exsr      impdet                                       => stessa sped.
023200110215     C                   endif
023300010305     C                   endif
023400110215     C*
023500051209     C                   endif
023600000905     C*
023700000905     C                   else
023800020725     C                   eval      x_vinflg = '1'
023900000905     C                   endif
024000000905     C*
024100020725     C     VINRRN        chain     tivin000
024200020725     C                   if        %found(tivin00r)
024300020725     C                   eval      y_vinflg = x_vinflg
024400020725     C                   eval      y_vinmsg = x_vinmsg
024500020725     C                   update    tivin000
024600020725     C                   endif
024700020725     C*
024800020725     C/EXEC SQL
024900051209     C+ Fetch C1 into :RECORDSETDS
025000020725     C/END-EXEC
025100020725     C*
025200020725     C                   enddo
025300020725     C*
025400020725     C/EXEC SQL
025500020725     C+ close C1
025600020725     C/END-EXEC
025700000905     C*
025800020305     C* Scarico i VAB rimasti "in sospeso"
025900020305     C                   exsr      wrivab
026000010202     C*
026100010202     C                   endif
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800010201     C                   if        §ctrokvb > 0
026900990921     C                   eval      wrkesito = '1'
027000000710     C                   else
027100000710     C                   eval      wrkesito = '2'
027200990910     C                   endif
027300000710     C                   endif
027400990910     C*
027500990914     C                   if        %open(tivin00r)
027600990908     C                   close     tivin00r
027700990914     C                   endif
027800110215     C                   if        %open(edivabwr)
027900110215     C                   close     edivabwr
028000990914     C                   endif
028100110215     C                   if        %open(edivatwr)
028200110215     C                   close     edivatwr
028300010201     C                   endif
028400990910     C*
028500010201     C                   if        §ctrokvb > 0
028600000724     C                             and vlrpoi <> *zeros
028700010202     C                   exsr      invio
028800990920     C                   endif
028900990920     C*
029000910830     C                   ENDSR
029100000613     C***
029200000801     C*----------------------------------------------------*
029300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400000801     C*----------------------------------------------------*
029500010201     C     INZVAR        BEGSR
029600000801     C*
029700051209     C                   Z-ADD     *zeros        Num5_0            5 0
029800051209     C                   Z-ADD     *zeros        wGiro             8 0
029900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030000020725     C                   MOVEL     '0'           FlgCAS            1
030100110215     C*
030200110215     C* Inizializzazione variabili x "gestione particolare"
030300110215     C                   Z-ADD     *zeros        idxDNS
030400110215     C                   Z-ADD     *zeros        idxWHOIS
030500110215     C                   CLEAR                   skSHIP
030600110215     C                   CLEAR                   skDNS
030700110215     C                   CLEAR                   skWHOIS
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031200000801     C*----------------------------------------------------*
031300020904     C     DEFCAM        BEGSR
031400000801     C*
031500110215     C                   CLEAR                   EDIVAB00
031600110215     C                   CLEAR                   EDIVAT00
031700110215     C                   Z-ADD     1161872       VABCCM
031800110215     C                   Z-ADD     1161872       VATCCM
031900110215     C                   Z-ADD     116           VABLNP
032000110215     C                   Z-ADD     116           VATLNP
032100110215     C                   Z-ADD     001           VABCTR
032200020305     C                   MOVEL     '7Q'          VABCTM
032300051209     C                   MOVEL     '1'           VABCBO
032400020305     C                   MOVEL     'E'           VATTRC
032500051212     C* ... e poi verifico se sono stati passati come parametri
032600051212     C                   IF        vlrppt > *blanks
032700051212     C                   IF        %subst(vlrppt:1:7) <> *blanks
032800051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032900051212     C                   EXSR      CHKNUM
033000051212     C                   IF        PiInt=*on
033100051212     C                   Z-ADD     PiVal         VABCCM
033200051212     C                   Z-ADD     PiVal         VATCCM
033300051212     C                   ENDIF
033400051212     C                   ENDIF
033500051212     C                   IF        %subst(vlrppt:8:3) <> *blanks
033600051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033700051212     C                   EXSR      CHKNUM
033800051212     C                   IF        PiInt=*on
033900051212     C                   Z-ADD     PiVal         VABLNP
034000051212     C                   Z-ADD     PiVal         VATLNP
034100051212     C                   ENDIF
034200051212     C                   ENDIF
034300051212     C                   IF        %subst(vlrppt:11:3) <> *blanks
034400051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034500051212     C                   EXSR      CHKNUM
034600051212     C                   IF        PiInt=*on
034700051212     C                   Z-ADD     PiVal         VABCTR
034800051212     C                   ENDIF
034900051212     C                   ENDIF
035000051212     C                   IF        %subst(vlrppt:14:2) <> *blanks
035100051212     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
035200051212     C                   ENDIF
035300051212     C                   ENDIF
035400000801     C*
035500000801     C                   ENDSR
035600000801     C*----------------------------------------------------*
035700110215     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
035800000801     C*----------------------------------------------------*
035900010201     C     IMPVAB        BEGSR
036000010305     C*
036100000801     C                   Z-ADD     *zeros        errore            1 0
036200021113     C*
036300021113     C                   MOVE(P)   vlrpoi        VABFGS
036400021113     C                   MOVE(P)   vlrpoi        VATFGS
036500051209     C*
036600051209     C* Valorizzo i campi data spedizione
036700051209     C                   MOVEL     datcor        VABAAS
036800051209     C                   MOVEL     datcor        VATAAS
036900051209     C                   MOVE      datcor        VABMGS
037000020725     C*
037100020725     C* Campi relativi al destinatario
037200051209     C                   EVAL      VABRSD=%trim(%subst(vindta:203:30))
037300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037500020117     C* ==
037600110215     C                   EVAL      VABRD2=%trim(%subst(vindta:401:15))
037700051209     C                   EVAL      VABIND=%trim(%subst(vindta:233:25))
037800051209     C                   EVAL      VABLOD=%trim(%subst(vindta:258:30))
037900051209     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
038000051209     C                   IF        %subst(vindta:1130:1) = 'E'
038100051209     C                   EVAL      VABTSP='E'
038200051209     C                   ENDIF
038300051209     C                   IF        %subst(vindta:46:2) = '02'
038400051209     C                   EVAL      VABCBO='2'
038500051209     C                   ENDIF
038600020725     C*
038700020725     C* Campi numerici
038800051209     C*
038900110215     C* ...VABNSP/VATNSP => Stacco un numeratore da AZNUM
039000060109     C                   clear                   TRUL33DS
039100060109     C                   eval      I33OPE = *zeros
039200060109     C                   eval      I33CNU = 302
039300060109     C                   eval      I33NUM = 1
039400060109     C                   movel     TRUL33DS      KPJBU
039500060109     C                   call      'TRUL33R'
039600060109     C                   parm                    KPJBA
039700060109     C                   movel     KPJBU         TRUL33DS
039800060109     C                   if        O33ERR = *zeros
039900060109     C                   z-add     O33NRF        VABNSP
040000060109     C                   z-add     O33NRF        VATNSP
040100060109     C                   else
040200060109     C                   Z-ADD     1             errore
040300060109     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400060109     C                             + ' ' + 'VABNSP VATNSP'
040500060109     C                   endif
040600051209     C* ...VABCAD
040700051209     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
040800000801     C                   EXSR      CHKNUM
040900000801     C                   IF        PiInt=*on
041000000801     C                   Z-ADD     PiVal         Num5_0
041100010208     C                   MOVEL(p)  Num5_0        VABCAD
041200000801     C                   ELSE
041300000801     C                   ADD       1             errore
041400020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041500000801     C                             + ' ' + 'VABCAD'
041600000801     C                   ENDIF
041700110216     C*
041800110216     C* Se provincia nn valorizzata la reperisco tramite TISI95
041900110218     C                   IF        VABPRD  = *blanks AND
042000110218     C                             VABCAD <> *blanks AND
042100110218     C                             VABLOD <> *blanks AND
042200110218     C                             VABNZD  = *blanks
042300110216     C                   CLEAR                   TISI95DS
042400110216     C                   EVAL      I95TCN = '3'
042500110216     C                   Z-ADD     datcor        I95DAT
042600110216     C                   EVAL      I95CAP = VABCAD
042700110216     C                   EVAL      I95LOC = VABLOD
042800110216     C                   CALL      'TISI95R'
042900110216     C                   PARM                    TISI95DS
043000110216     C                   EVAL      VABPRD = O95PRV
043100110216     C                   ENDIF
043200051209     C* ...VABNCL
043300110215     C***                EVAL      PiStr=%trim(%subst(vindta:335:4))
043400110215     C***                EXSR      CHKNUM
043500110215     C***                IF        PiInt=*on
043600110215     C***                ADD       PiVal         VABNCL
043700110215     C***                ELSE
043800110215     C***                ADD       1             errore
043900110215     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
044000110215     C***                          + ' ' + 'VABNCL'
044100110215     C***                ENDIF
044200051209     C* ...VABPKB
044300051209     C                   EVAL      PiStr=%trim(%subst(vindta:371:8))
044400051209     C                   EXSR      CHKNUM
044500051209     C                   IF        PiNum=*on
044600051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
044700051209     C                   ADD(H)    PiVal         VABPKB
044800051209     C                   ELSE
044900051209     C                   ADD       1             errore
045000051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100051209     C                             + ' ' + 'VABPKB'
045200051209     C                   ENDIF
045300101122     C* ...VABVLB
045400101122     C                   EVAL      PiStr=%trim(%subst(vindta:363:8))
045500101122     C                   EXSR      CHKNUM
045600101122     C                   IF        PiNum=*on
045700101122     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
045800101122     C                   ADD(H)    PiVal         VABVLB
045900101122     C                   ELSE
046000101122     C                   ADD       1             errore
046100101122     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200101122     C                             + ' ' + 'VABVLB'
046300101122     C                   ENDIF
046400051209     C* ...VABCAS
046500051209     C                   IF        %trim(%subst(vindta:681:10)) <> *zeros
046600051209     C                   EVAL      FlgCAS='1'
046700051209     C                   EVAL      VABVCA='EUR'
046800051209     C                   EVAL      PiStr=%trim(%subst(vindta:681:10))
046900051209     C                   EXSR      CHKNUM
047000051209     C                   IF        PiNum=*on
047100051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
047200051209     C                   ADD(H)    PiVal         VABCAS
047300051209     C                   ELSE
047400051209     C                   ADD       1             errore
047500051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600051209     C                             + ' ' + 'VABCAS'
047700051209     C                   ENDIF
047800051209     C                   ENDIF
047900051209     C*
048000051209     C                   ENDSR
048100110215     C*----------------------------------------------------*
048200110215     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB) - DETTAGLI
048300110215     C*----------------------------------------------------*
048400110215     C     IMPDET        BEGSR
048500110215     C*
048600110215     C* Gestione schiere "particolari" x RIFs, DNs e BARCODES
048700110215     C* ...VABRMN
048800110215     C                   IF        %subst(vindta:416:15)<>%subst(vindta:401:15)
048900110215     C                   EVAL      PiStr=%trim(%subst(vindta:416:15))
049000110215     C                   EXSR      CHKNUM
049100110215     C                   IF        PiInt=*on
049200110215     C                   Z-ADD     PiVal         skSHIP(1)
049300110215     C                   ELSE
049400110215     C                   ADD       1             errore
049500110215     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600110215     C                             + ' ' + 'VABRMN'
049700110215     C                   ENDIF
049800110215     C                   ENDIF
049900110215     C*
050000110215     C                   movel     *blanks       wDN              15
050100110215     C                   eval      wDN = %trim(%subst(vindta:431:15))
050200110215     C                   if        %lookup(wDN:skDNS:1) = 0
050300110215     C                   add       1             idxDNS
050400110215     C                   eval      skDNS(idxDNS) = wDN
050500110215     C                   endif
050600110215     C*
050700110215     C                   movel     *blanks       wWHOIS           20
050800110215     C                   eval      wWHOIS = %trim(%subst(vindta:1:20))
050900110215     C                   if        %lookup(wWHOIS:skWHOIS:1) = 0
051000110215     C                   add       1             idxWHOIS
051100110215     C                   add       1             VABNCL
051200110215     C                   eval      skWHOIS(idxWHOIS) = wWHOIS
051300110215     C                   endif
051400110215     C*
051500110215     C                   ENDSR
051600110215     C*----------------------------------------------------*
051700110215     C*  AZZERAMENTO CAMPI NN RIFERITI AL DESTINATARIO / RIFERIMENTI
051800110215     C*----------------------------------------------------*
051900110215     C     AZZDAT        BEGSR
052000110215     C*
052100110215     C                   EVAL      vabNCL = *zeros
052200110215     C                   EVAL      vabPKB = *zeros
052300110215     C                   EVAL      vabVLB = *zeros
052400110215     C                   EVAL      vabQFT = *zeros
052500110215     C                   EVAL      vabVMD = *zeros
052600110215     C                   EVAL      vabCAS = *zeros
052700110215     C                   EVAL      vabIAS = *zeros
052800110215     C*
052900110215     C                   ENDSR
053000110215
053100110215     C*----------------------------------------------------*
053200110215     C*  SCARICAMENTO BUFFER RECORDS VAB
053300110215     C*----------------------------------------------------*
053400110215     C     WRIVAB        BEGSR
053500110215     C*
053600110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
053700110215     C                   EVAL      VABCMR = 'ACCORPARE SEMPRE - ' +
053800110215     C                                      %char(datcor)+' '+%char(oracor)
053900110215     C                   EVAL      VABDCM = datcor
054000110215     C                   EVAL      VABDTS = datcor
054100110215     C                   EVAL      VABHMS = oracor
054200110215     C                   EVAL      VABCNT = 1
054300110215     C*
054400110215     C* Considerazioni sul contenuto di campi precedentemente valorizzati
054500110215     C                   IF        FlgCAS <> '0'
054600110215     C                   IF        VABCBO = '1'
054700110215     C                   EVAL      VABCBO = '4'
054800110215     C                   ELSE
054900110215     C                   EVAL      VABCBO = '6'
055000110215     C                   ENDIF
055100110215     C                   ENDIF
055200110215     C*
055300110215     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055400110215     C                   EXSR      CHKIMPDIV
055500110215     C*
055600110215     C* Ebbene...
055700110215     C                   ADD       1             §CTRMO
055800110215     C                   IF        errore <> *zeros
055900110215     C                   ADD       1             §CTRNO
056000110215     C                   EVAL      recko = vindta
056100110215     C                   EXCEPT    rigadett
056200110215     C                   EVAL      x_vinflg = '2'
056300110215     C                   ELSE
056400110215     C                   ADD       1             §CTROKVB
056500110215     C                   ENDIF
056600110215     C*
056700110215     C* Scarico il buffer della bolla "master"
056800110215     C                   EVAL      VABRMN=skSHIP(1)
056900110215     C                   EVAL      VABRMA=skDNS(1)
057000110215     C                   write     edivab00                                     => scarico il VAB
057100110215     C*
057200110215     C* Scarico il buffer dei "chi sono" della bolla "master"
057300110215     C                   exsr      wrivat
057400110215     C*
057500110215     C* Scarico il buffer delle bolle "slave"
057600110215     C                   exsr      wrislave
057700110215     C*
057800110215     C                   ENDSR
057900110215
058000010201     C*----------------------------------------------------*
058100110215     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
058200010201     C*----------------------------------------------------*
058300051209     C     WRIVAT        BEGSR
058400110215     C*
058500110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
058600110215     C                   EVAL      VATCMR = 'ACCORPARE SEMPRE - ' +
058700110215     C                                      %char(datcor)+' '+%char(oracor)
058800110215     C                   EVAL      VATCNT = 1
058900110215     C*
059000110215     C* Ciclo x tutti i "chi sono" della spedizione e li attacco alla
059100110215     C* spedizione "master"
059200110215     C                   z-add     1             i                 4 0
059300110215     C*
059400110215     C                   dow       i <= idxWHOIS
059500110215     C                   eval      VATTRC='E'
059600110215     C                   eval      VATNOT = skWHOIS(i)
059700110215     C                   write     EDIVAT00
059800110215     C                   ADD       1             §CTROKVB
059900110215     C*
060000110215     C                   add       1             i
060100110215     C                   enddo
060200010201     C*
060300010201     C                   ENDSR
060400020904
060500020904
060600110215     C*----------------------------------------------------*
060700110215     C*  CONSIDERAZIONI E SCRITTURA RECORD TESTATA BOLLE "SLAVE"
060800110215     C*----------------------------------------------------*
060900110215     C     WRISLAVE      BEGSR
061000110215     C*
061100110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
061200110215     C                   EVAL      VABCMR = 'ACCORPARE SEMPRE - ' +
061300110215     C                                      %char(datcor)+' '+%char(oracor)
061400110215     C                   EVAL      VABDCM = datcor
061500110215     C                   EVAL      VABDTS = datcor
061600110215     C                   EVAL      VABHMS = oracor
061700110215     C                   EVAL      VABCNT = 1
061800110215     C*
061900110215     C* Ciclo x tutte le DNs della spedizione e genero tante bolle "slave"
062000110215     C                   z-add     2             i                 4 0
062100110215     C*
062200110215     C                   dow       i <= idxDNS
062300110215     C* ...VABNSP => Stacco un numeratore da AZNUM
062400110215     C                   clear                   TRUL33DS
062500110215     C                   eval      I33OPE = *zeros
062600110215     C                   eval      I33CNU = 302
062700110215     C                   eval      I33NUM = 1
062800110215     C                   movel     TRUL33DS      KPJBU
062900110215     C                   call      'TRUL33R'
063000110215     C                   parm                    KPJBA
063100110215     C                   movel     KPJBU         TRUL33DS
063200110215     C                   if        O33ERR = *zeros
063300110215     C                   z-add     O33NRF        VABNSP
063400110215     C*
063500110215     C* Se tutto ok => procedo
063600110215     C                   exsr      azzdat
063700110215     C                   EVAL      VABRMN=skSHIP(1)
063800110215     C                   EVAL      VABRMA=skDNS(i)
063900110215     C                   EVAL      VABNOT=skDNS(i)
064000110215     C                   write     edivab00                                     => scarico il VAB
064100110215     C                   ADD       1             §CTROKVB
064200110215     C*
064300110215     C                   else
064400110215     C                   Z-ADD     1             errore
064500110215     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
064600110215     C                             + ' ' + '"SLAVE"'
064700110215     C                   endif
064800110215     C*
064900110215     C                   add       1             i
065000110215     C                   enddo
065100110215     C*
065200110215     C                   ENDSR
065300110215
065400110215
065500010202     C*----------------------------------------------------*
065600110215     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
065700010202     C*----------------------------------------------------*
065800020305     C     PREVAT        BEGSR
065900010202     C*
066000110215     C* Compongo il nome del membro da dare al EDIVATWR
066100010202     C                   eval      parmbr = vlrhdl
066200010202     C                   movel     'M'           parmbr
066300051209     C                   eval      parccm = vlrksc
066400010202     C                   eval      paropz = '1'
066500010202     C* Effettuo la chiamata al CLLE preposto
066600110215     C                   call(e)   'TITVEVTC'
066700010202     C                   parm                    parccm
066800010202     C                   parm                    parmbr
066900010202     C                   parm                    paropz
067000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067100010202     C                   if        %error
067200010202     C                   movel     '1'           chkcall
067300010202     C                   else
067400010202     C                   movel     '0'           chkcall
067500010202     C                   endif
067600010202     C*
067700010202     C                   ENDSR
067800000801     C*----------------------------------------------------*
067900000801     C*  CONTROLLO NUMERICITA' CAMPI
068000000801     C*----------------------------------------------------*
068100000801     C     CHKNUM        BEGSR
068200000801     C*
068300000801     C                   call(e)   'ISNUMERIC'
068400000801     C                   PARM                    PiStr            30
068500000801     C                   PARM      ','           PiDecChr          1
068600000801     C                   PARM      *ZEROS        PiVal            30 9
068700000801     C                   PARM      '0'           PiInt             1
068800000801     C                   PARM      '0'           PiNum             1
068900000801     C                   IF        %error
069000000801     C                   EVAL      PiInt=*off
069100000801     C                   ENDIF
069200000801     C*
069300000801     C                   ENDSR
069400000801     C***
069500000801
069600011113
069700011113     C*----------------------------------------------------*
069800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069900011113     C*----------------------------------------------------*
070000011113     C     CHKIMPDIV     BEGSR
070100011113     C*
070200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070300011113     C                   Z-ADD     *zeros        wrkDec            9 9
070400011113     C*
070500011113     C* Come prima cosa effettuo considerazioni sulla divisa
070600011113     C                   IF        vabIAS > *zeros
070700011113     C                   IF        vabVAS <> 'EUR'
070800011113     C                   EVAL      vabVAS =  'ITL'
070900011113     C                   ENDIF
071000011113     C                   ENDIF
071100011113     C*
071200011113     C                   IF        vabCAS > *zeros
071300011113     C                   IF        vabVCA <> 'EUR'
071400011113     C                   EVAL      vabVCA =  'ITL'
071500011113     C                   ENDIF
071600011113     C                   ENDIF
071700011113     C*
071800011113     C                   IF        vabVMD > *zeros
071900020305     C                   IF        vabVAD <> 'EUR'
072000011113     C                   EVAL      vabVAD =  'ITL'
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072500011113     C                   Z-ADD     vabIAS        wrkDec
072600011113     C                   IF        wrkDec > *zeros
072700011113     C                   IF        vabVAS = 'ITL'
072800011113     C                   EVAL      vabIAS = *zeros
072900011113     C                   ENDIF
073000011113     C                   ENDIF
073100011113     C*
073200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073300011113     C                   Z-ADD     vabCAS        wrkDec
073400011113     C                   IF        wrkDec > *zeros
073500011113     C                   IF        vabVCA = 'ITL'
073600011113     C                   EVAL      vabCAS = *zeros
073700011113     C                   ENDIF
073800011113     C                   ENDIF
073900011113     C*
074000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074100011113     C                   Z-ADD     vabVMD        wrkDec
074200011113     C                   IF        wrkDec > *zeros
074300011113     C                   IF        vabVAD = 'ITL'
074400011113     C                   EVAL      vabVMD = *zeros
074500011113     C                   ENDIF
074600011113     C                   ENDIF
074700011113     C*
074800011113     C                   ENDSR
074900011113     C***
075000011113
075100011113
075200000801
075300000801
075400990920      /TITLE Invio dei dati al punto operativo.
075500010202     C     invio         BEGSR
075600990920     C*
075700110215     C* 1° invio EDIVAT
075800010201     C                   reset                   dscmz
075900010201     C                   move      vlrpoi        cmzdst
076000110215     C                   eval      cmzfld = 'EDIVATWR'
076100010201     C                   eval      cmzmbd = vlrhdl
076200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076300021009     C***                if        prmfir = *blanks
076400110215     C                   eval      cmzfla = 'EDIVAT0F'
076500110215     C                   eval      cmzmba = 'EDIVAT0F'
076600021009     C***                else
076700021009     C***                eval      cmzfla = prmfir
076800021009     C***                eval      cmzmba = prmfir
076900021009     C***                endif
077000010201     C                   eval      cmznrr = *zeros
077100020305     C                   move      §ctrokvt      cmznrr
077200021018     C                   eval      cmzlba = vlrfl1
077300010201     C                   call(e)   'TIS711C'
077400010201     C                   parm                    dscmz
077500010201     C                   parm      *blanks       esito
077600010205     C                   if        %error
077700010205     C                             or cmzerr = '1'
077800010205     C                             or esito  = '1'
077900010205     C                   eval      wrkesito = '3'
078000010205     C                   else
078100010201     C*
078200110215     C* 2° invio EDIVAB
078300010201     C                   reset                   dscmz
078400010201     C                   move      vlrpoi        cmzdst
078500010201     C                   eval      cmzfld = vlrfou
078600010201     C                   eval      cmzmbd = vlrhdl
078700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078800021009     C***                if        prmfir = *blanks
078900110215     C                   eval      cmzfla = 'EDIVAB0F'
079000110215     C                   eval      cmzmba = 'EDIVAB0F'
079100021009     C***                else
079200021009     C***                eval      cmzfla = prmfir
079300021009     C***                eval      cmzmba = prmfir
079400021009     C***                endif
079500010201     C                   eval      cmznrr = *zeros
079600010201     C                   move      §ctrokvb      cmznrr
079700021018     C                   eval      cmzlba = vlrfl1
079800010201     C                   call(e)   'TIS711C'
079900010201     C                   parm                    dscmz
080000010201     C                   parm      *blanks       esito
080100010201     C                   if        %error
080200010201     C                             or cmzerr = '1'
080300010201     C                             or esito  = '1'
080400010201     C                   eval      wrkesito = '3'
080500010201     C                   endif
080600010205     C                   endif
080700990920     C*
080800000613     C                   ENDSR
080900000613     C***
081000070411
081100070411     C     *pssr         BEGSR
081200070411     C*
081300070411     C                   if        %open(tivin00r)
081400070411     C                   close     tivin00r
081500070411     C                   endif
081600110215     C                   if        %open(edivabwr)
081700110215     C                   close     edivabwr
081800070411     C                   endif
081900110215     C                   if        %open(edivatwr)
082000110215     C                   close     edivatwr
082100070411     C                   endif
082200070411     C*
082300070411     C* Effettuo la chiamata al CLLE preposto
082400110215     C                   call(e)   'TITVEVTC'
082500070411     C                   parm                    parccm
082600070411     C                   parm                    parmbr
082700070411     C                   parm      '2'           paropz
082800070411     C*
082900070411     C                   eval      wrkesito = '2'
083000070411     C*
083100070411     C                   seton                                        LR
083200070411     C*
083300070411     C                   ENDSR     '*CANCL'
083400070411     C***
083500990910
083600000613     C     *inzsr        BEGSR
083700990910     C*
083800990910     C     *entry        plist
083900990920     C                   parm                    tivlrds
084000990921     C                   parm      wrkesito      esito
084100000724     C                   parm                    prmlit
084200000710     C                   parm                    prmfir
084300000613     C*
084400000830     C* CALCOLA LA DATA CORRENTE
084500110215     C                   time                    wn14             14 0
084600110215     C                   movel     wn14          oracor            6 0          *ORA
084700110215     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084800110215     C                   eval      datcor = %dec(%date() : *ISO)
084900000830     C*
085000000613     C                   ENDSR
085100000613     C***
085200990908
085300101122     Otitv2b4p  E            riepilogo         2
085400990915     O                                              'Upload via Internet'
085500990915     O                                           +1 'Traduzione TIVIN00R -'
085600110215     O                                           +1 'EDIVABWR/EDIVATWR'
085700010201     O                                           +1 'Report testate bolle'
085800990915     O          E            riepilogo   2
085900990915     O                       wrkdata
086000990915     O                       wrkora              +1
086100990915     O                       procname            +1
086200990915     O          E            riepilogo   2
086300990915     O                                              'Cliente..................:'
086400990915     O                       VABCCM        z     +1
086500990915     O          E            riepilogo   2
086600990920     O                                              'Riferimento Strategi.....:'
086700990920     O                       vlrhdl              +1
086800990915     O          E            riepilogo   2
086900990915     O                                              'Giusti...................:'
087000010201     O                       §CTROKVB      2   +  1
087100990915     O          E            riepilogo   2
087200990915     O                                              'Sbagliati e corretti.....:'
087300971022     O                       §CTRMO        2   +  1
087400990915     O          E            riepilogo   2
087500990915     O                                              'Sbagliati e scartati.....:'
087600971022     O                       §CTRNO        2   +  1
087700000613
087800101122     Otitv2b4ps E            testdett          2
087900000613     O                                              'Upload via Internet'
088000000613     O                                           +1 'Traduzione TIVIN00R -'
088100110215     O                                           +1 'EDIVABWR/EDIVATWR'
088200010201     O                                           +1 'Report testate bolle'
088300000616     O          E            testdett    3
088400000613     O                                           +2 'N° rec'
088500000613     O                                           +3 'Anteprima contenuto'
088600000616     O          E            rigadett    2
088700000613     O                       rrnum               +2
088800000621     O                       recko               +3
088900000616     O          E            findett     2
089000000613     O                       wrkdata
089100000613     O                       wrkora              +1
089200000613     O                       procname            +1
089300000616     O          E            findett     2
089400000613     O                                              'Cliente..................:'
089500000613     O                       VABCCM        z     +1
089600000616     O          E            findett     2
089700000613     O                                              'Riferimento Strategi.....:'
089800000613     O                       vlrhdl              +1
089900000616     O          E            findett     2
090000000613     O                                              'Giusti...................:'
090100010201     O                       §CTROKVB      2   +  1
090200000616     O          E            findett     2
090300000613     O                                              'Sbagliati e corretti.....:'
090400000613     O                       §CTRMO        2   +  1
090500000616     O          E            findett     2
090600000613     O                                              'Sbagliati e scartati.....:'
090700000613     O                       §CTRNO        2   +  1
090800000512** CMD - COMANDI CL
090900101122OVRPRTF FILE(TITV2B4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
091000101122OVRPRTF FILE(TITV2B4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
091100101122DLTOVR FILE(TITV2B4P TITV2B4PS) LVL(*)
091200000512
091300000512
