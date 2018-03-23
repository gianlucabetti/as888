000100110215      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500110215     FEDIVABwr  O    E             DISK    usropn
000600110215     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500110216     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100131111     D depspe          s             10    INZ(*blanks)
002200051209     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002700110215     D skSHIP          s             15  0 INZ(*blanks) dim(1000)
002800110215     D skDNS           s             15    INZ(*blanks) dim(1000)
002900110215     D idxDNS          s              4  0 INZ(*zeros)
003000110215     D skWHOIS         s             20    INZ(*blanks) dim(1000)
003100110215     D idxWHOIS        s              4  0 INZ(*zeros)
003200020725
003300020725     D*------------------
003400051209     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500020725     D*------------------
003600051209     D RECORDSETDS     DS                  INZ
003700051209     D  VINDTA                     2048
003800051209     D  VINFLG                        1
003900131108     D  VINSPED                      10
004000110215     D  VINDNS                       15
004100051209     D  VINRRN                        8  0
004200060109     D*------------------
004300060109     D* DS REPERIMENTO NUMERATORE
004400060109     D*------------------
004500060109     D trul33ds      e ds                  inz
004600060109     D*------------------
004700060109     D* DS ARCHITETTURA
004800060109     D*------------------
004900060109     D kpjba         e ds                  inz
005000060109     D*------------------
005100020725     D*
005200990908
005300010201
005400010201
005500120309     C*
005600120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005700120309     C
005800120309     C/EXEC SQL
005900120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006000120309     C/END-EXEC
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400000613     C*
006500051209     C                   EXSR      RWVAB                                        LETT/SCR. VAB
006600000613     C*
006700010202     C* Effettuo la chiamata al CLLE preposto
006800110215     C                   call(e)   'TITVEVTC'
006900010202     C                   parm                    parccm
007000010202     C                   parm                    parmbr
007100010202     C                   parm      '2'           paropz
007200110216     C*
007300110216     C* Effettuo lancio TISI95 solo x chiusura
007400110216     C                   CLEAR                   TISI95DS
007500110216     C                   EVAL      I95TLA = 'C'
007600110216     C                   CALL      'TISI95R'
007700110216     C                   PARM                    TISI95DS
007800000616     C*
007900000801     C
008000010201     C                   seton                                        LR
008100990908
008200000801
008300910830     C*--------------------------------------------------------
008400110215     C* RWVAB    LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008500910830     C*--------------------------------------------------------
008600051209     C     RWVAB         BEGSR
008700990910     C*
008800990914     C                   if        not %open(tivin00r)
008900990908     C                   open      tivin00r
009000990914     C                   endif
009100110215     C                   if        not %open(edivabwr)
009200110215     C                   open      edivabwr
009300990914     C                   endif
009400110215     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009500020305     C                   exsr      prevat
009600010201     C*
009700010202     C                   if        chkcall = '0'
009800010202     C*
009900110215     C                   if        not %open(edivatwr)
010000110215     C                   open      edivatwr
010100010201     C                   endif
010200990910     C*
010300010201     C                   clear                   §CTROKVB          5 0
010400020305     C                   clear                   §CTROKVT          5 0
010500000801     C                   clear                   §CTRMO            5 0
010600000801     C                   clear                   §CTRNO            5 0
010700990910     C*
010800020725     C*
010900020725     C/EXEC SQL
011000020725     C+ declare C1 cursor for select
011100131108     C+ vindta, vinflg, substr(vindta, 446, 10) as sped,
011200110215     C+ substr(vindta, 431, 15) as dns,
011300051209     C+ rrn(tivin00r)
011400110215     C+ from tivin00r order by sped, dns
011500020725     C+ for read only
011600020725     C/END-EXEC
011700020725     C
011800020725     C/EXEC SQL
011900020725     C+ open C1
012000020725     C/END-EXEC
012100020725     C
012200020725     C/EXEC SQL
012300051209     C+ Fetch C1 into :RECORDSETDS
012400020725     C/END-EXEC
012500020725     C*
012600020725     C                   dow       sqlcod = *zeros
012700990913     C*
012800020725     C                   if        vindta > *blanks
012900000613     C                   add       1             rrnum
013000000801     C*
013100020725     C                   if        vinflg = *blanks
013200020725     C                             or vinflg = '0'
013300020725     C                             or vinflg = '2'
013400000801     C*
013500020725     C                   clear                   x_vinmsg
013600020725     C                   eval      x_vinflg = '1'
013700010305     C*
013800051209     C* A rottura d destinatario eseguo operazioni
013900110215     C                   if        depspe  = *blanks                            => 1° giro
014000110215     C                   eval      depspe  = VINSPED
014100020725     C                   clear                   tivinds
014200020725     C                   exsr      inzvar
014300020725     C                   exsr      defcam
014400020305     C                   exsr      impvab
014500110215     C                   exsr      impdet
014600010305     C                   else
014700110215     C                   if        depspe  <> VINSPED                           => rottura
014800110215     C                   eval      depspe   = VINSPED                           => memorizz. new rot
014900020305     C                   exsr      wrivab
015000020725     C                   clear                   tivinds
015100020725     C                   exsr      inzvar
015200020725     C                   exsr      defcam
015300020305     C                   exsr      impvab
015400110215     C                   exsr      impdet
015500110215     C                   else
015600110215     C                   exsr      impdet                                       => stessa sped.
015700110215     C                   endif
015800010305     C                   endif
015900110215     C*
016000051209     C                   endif
016100000905     C*
016200000905     C                   else
016300020725     C                   eval      x_vinflg = '1'
016400000905     C                   endif
016500000905     C*
016600020725     C     VINRRN        chain     tivin000
016700020725     C                   if        %found(tivin00r)
016800020725     C                   eval      y_vinflg = x_vinflg
016900020725     C                   eval      y_vinmsg = x_vinmsg
017000020725     C                   update    tivin000
017100020725     C                   endif
017200020725     C*
017300020725     C/EXEC SQL
017400051209     C+ Fetch C1 into :RECORDSETDS
017500020725     C/END-EXEC
017600020725     C*
017700020725     C                   enddo
017800020725     C*
017900020725     C/EXEC SQL
018000020725     C+ close C1
018100020725     C/END-EXEC
018200000905     C*
018300020305     C* Scarico i VAB rimasti "in sospeso"
018400020305     C                   exsr      wrivab
018500010202     C*
018600010202     C                   endif
018700990910
018800990910     C* Se non ci sono record con errori ...
018900000710     C                   if        §ctrno = 0
019000990910     C* ... restituisco esito OK.
019100990921     C                   eval      wrkesito = '0'
019200990910     C                   else
019300010201     C                   if        §ctrokvb > 0
019400990921     C                   eval      wrkesito = '1'
019500000710     C                   else
019600000710     C                   eval      wrkesito = '2'
019700990910     C                   endif
019800000710     C                   endif
019900990910     C*
020000990914     C                   if        %open(tivin00r)
020100990908     C                   close     tivin00r
020200990914     C                   endif
020300110215     C                   if        %open(edivabwr)
020400110215     C                   close     edivabwr
020500990914     C                   endif
020600110215     C                   if        %open(edivatwr)
020700110215     C                   close     edivatwr
020800010201     C                   endif
020900990910     C*
021000010201     C                   if        §ctrokvb > 0
021100000724     C                             and vlrpoi <> *zeros
021200010202     C                   exsr      invio
021300990920     C                   endif
021400990920     C*
021500910830     C                   ENDSR
021600000613     C***
021700000801     C*----------------------------------------------------*
021800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021900000801     C*----------------------------------------------------*
022000010201     C     INZVAR        BEGSR
022100000801     C*
022200051209     C                   Z-ADD     *zeros        Num5_0            5 0
022300051209     C                   Z-ADD     *zeros        wGiro             8 0
022400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022500020725     C                   MOVEL     '0'           FlgCAS            1
022600110215     C*
022700110215     C* Inizializzazione variabili x "gestione particolare"
022800110215     C                   Z-ADD     *zeros        idxDNS
022900110215     C                   Z-ADD     *zeros        idxWHOIS
023000110215     C                   CLEAR                   skSHIP
023100110215     C                   CLEAR                   skDNS
023200110215     C                   CLEAR                   skWHOIS
023300000801     C*
023400000801     C                   ENDSR
023500000801     C*----------------------------------------------------*
023600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
023700000801     C*----------------------------------------------------*
023800020904     C     DEFCAM        BEGSR
023900000801     C*
024000110215     C                   CLEAR                   EDIVAB00
024100110215     C                   CLEAR                   EDIVAT00
024200110215     C                   Z-ADD     1161872       VABCCM
024300110215     C                   Z-ADD     1161872       VATCCM
024400110215     C                   Z-ADD     116           VABLNP
024500110215     C                   Z-ADD     116           VATLNP
024600110215     C                   Z-ADD     001           VABCTR
024700020305     C                   MOVEL     '7Q'          VABCTM
024800051209     C                   MOVEL     '1'           VABCBO
024900020305     C                   MOVEL     'E'           VATTRC
025000051212     C* ... e poi verifico se sono stati passati come parametri
025100051212     C                   IF        vlrppt > *blanks
025200051212     C                   IF        %subst(vlrppt:1:7) <> *blanks
025300051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025400051212     C                   EXSR      CHKNUM
025500051212     C                   IF        PiInt=*on
025600051212     C                   Z-ADD     PiVal         VABCCM
025700051212     C                   Z-ADD     PiVal         VATCCM
025800051212     C                   ENDIF
025900051212     C                   ENDIF
026000051212     C                   IF        %subst(vlrppt:8:3) <> *blanks
026100051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026200051212     C                   EXSR      CHKNUM
026300051212     C                   IF        PiInt=*on
026400051212     C                   Z-ADD     PiVal         VABLNP
026500051212     C                   Z-ADD     PiVal         VATLNP
026600051212     C                   ENDIF
026700051212     C                   ENDIF
026800051212     C                   IF        %subst(vlrppt:11:3) <> *blanks
026900051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000051212     C                   EXSR      CHKNUM
027100051212     C                   IF        PiInt=*on
027200051212     C                   Z-ADD     PiVal         VABCTR
027300051212     C                   ENDIF
027400051212     C                   ENDIF
027500051212     C                   IF        %subst(vlrppt:14:2) <> *blanks
027600051212     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
027700051212     C                   ENDIF
027800051212     C                   ENDIF
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200110215     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
028300000801     C*----------------------------------------------------*
028400010201     C     IMPVAB        BEGSR
028500010305     C*
028600000801     C                   Z-ADD     *zeros        errore            1 0
028700021113     C*
028800021113     C                   MOVE(P)   vlrpoi        VABFGS
028900021113     C                   MOVE(P)   vlrpoi        VATFGS
029000051209     C*
029100051209     C* Valorizzo i campi data spedizione
029200051209     C                   MOVEL     datcor        VABAAS
029300051209     C                   MOVEL     datcor        VATAAS
029400051209     C                   MOVE      datcor        VABMGS
029500020725     C*
029600020725     C* Campi relativi al destinatario
029700051209     C                   EVAL      VABRSD=%trim(%subst(vindta:203:30))
029800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030000020117     C* ==
030100131010     C***                EVAL      VABRD2=%trim(%subst(vindta:401:15))
030200051209     C                   EVAL      VABIND=%trim(%subst(vindta:233:25))
030300051209     C                   EVAL      VABLOD=%trim(%subst(vindta:258:30))
030400051209     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
030500051209     C                   IF        %subst(vindta:1130:1) = 'E'
030600051209     C                   EVAL      VABTSP='E'
030700051209     C                   ENDIF
030800051209     C                   IF        %subst(vindta:46:2) = '02'
030900051209     C                   EVAL      VABCBO='2'
031000051209     C                   ENDIF
031100020725     C*
031200020725     C* Campi numerici
031300051209     C*
031400110215     C* ...VABNSP/VATNSP => Stacco un numeratore da AZNUM
031500060109     C                   clear                   TRUL33DS
031600060109     C                   eval      I33OPE = *zeros
031700060109     C                   eval      I33CNU = 302
031800060109     C                   eval      I33NUM = 1
031900060109     C                   movel     TRUL33DS      KPJBU
032000060109     C                   call      'TRUL33R'
032100060109     C                   parm                    KPJBA
032200060109     C                   movel     KPJBU         TRUL33DS
032300060109     C                   if        O33ERR = *zeros
032400060109     C                   z-add     O33NRF        VABNSP
032500060109     C                   z-add     O33NRF        VATNSP
032600060109     C                   else
032700060109     C                   Z-ADD     1             errore
032800060109     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
032900060109     C                             + ' ' + 'VABNSP VATNSP'
033000060109     C                   endif
033100051209     C* ...VABCAD
033200051209     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
033300000801     C                   EXSR      CHKNUM
033400000801     C                   IF        PiInt=*on
033500000801     C                   Z-ADD     PiVal         Num5_0
033600010208     C                   MOVEL(p)  Num5_0        VABCAD
033700000801     C                   ELSE
033800000801     C                   ADD       1             errore
033900020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
034000000801     C                             + ' ' + 'VABCAD'
034100000801     C                   ENDIF
034200110216     C*
034300110216     C* Se provincia nn valorizzata la reperisco tramite TISI95
034400110218     C                   IF        VABPRD  = *blanks AND
034500110218     C                             VABCAD <> *blanks AND
034600110218     C                             VABLOD <> *blanks AND
034700110218     C                             VABNZD  = *blanks
034800110216     C                   CLEAR                   TISI95DS
034900110216     C                   EVAL      I95TCN = '3'
035000110216     C                   Z-ADD     datcor        I95DAT
035100110216     C                   EVAL      I95CAP = VABCAD
035200110216     C                   EVAL      I95LOC = VABLOD
035300110216     C                   CALL      'TISI95R'
035400110216     C                   PARM                    TISI95DS
035500110216     C                   EVAL      VABPRD = O95PRV
035600110216     C                   ENDIF
035700051209     C* ...VABNCL
035800110215     C***                EVAL      PiStr=%trim(%subst(vindta:335:4))
035900110215     C***                EXSR      CHKNUM
036000110215     C***                IF        PiInt=*on
036100110215     C***                ADD       PiVal         VABNCL
036200110215     C***                ELSE
036300110215     C***                ADD       1             errore
036400110215     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
036500110215     C***                          + ' ' + 'VABNCL'
036600110215     C***                ENDIF
036700051209     C* ...VABCAS
036800051209     C                   IF        %trim(%subst(vindta:681:10)) <> *zeros
036900051209     C                   EVAL      FlgCAS='1'
037000051209     C                   EVAL      VABVCA='EUR'
037100051209     C                   EVAL      PiStr=%trim(%subst(vindta:681:10))
037200051209     C                   EXSR      CHKNUM
037300051209     C                   IF        PiNum=*on
037400051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
037500051209     C                   ADD(H)    PiVal         VABCAS
037600051209     C                   ELSE
037700051209     C                   ADD       1             errore
037800051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037900051209     C                             + ' ' + 'VABCAS'
038000051209     C                   ENDIF
038100051209     C                   ENDIF
038200051209     C*
038300051209     C                   ENDSR
038400110215     C*----------------------------------------------------*
038500110215     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB) - DETTAGLI
038600110215     C*----------------------------------------------------*
038700110215     C     IMPDET        BEGSR
038800110215     C*
038900110215     C* Gestione schiere "particolari" x RIFs, DNs e BARCODES
039000110215     C* ...VABRMN
039100131111     C                   IF        %trim(%subst(vindta:416:15)) <>
039200131111     C                             %trim(%subst(vindta:446:10))
039300110215     C                   EVAL      PiStr=%trim(%subst(vindta:416:15))
039400110215     C                   EXSR      CHKNUM
039500110215     C                   IF        PiInt=*on
039600110215     C                   Z-ADD     PiVal         skSHIP(1)
039700110215     C                   ELSE
039800110215     C                   ADD       1             errore
039900110215     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040000110215     C                             + ' ' + 'VABRMN'
040100110215     C                   ENDIF
040200110215     C                   ENDIF
040300110215     C*
040400110215     C                   movel     *blanks       wDN              15
040500110215     C                   eval      wDN = %trim(%subst(vindta:431:15))
040600110215     C                   if        %lookup(wDN:skDNS:1) = 0
040700110215     C                   add       1             idxDNS
040800110215     C                   eval      skDNS(idxDNS) = wDN
040900110215     C                   endif
041000110323     C*
041100110323     C* Forzatura sul codice cliente mittente
041200140714     C***                if        %subst(vindta:431:3) = '008'
041300140714     C                   if        %subst(vindta:431:3) = '005'
041400110323     C                   eval      VABCCM = 1161872
041500110323     C                   eval      VATCCM = 1161872
041600110323     C                   endif
041700170119     C                   if        %subst(vindta:431:3) = '080' OR
041800170119     C                             %subst(vindta:431:3) = '007'
041900110323     C                   eval      VABCCM = 1161985
042000110323     C                   eval      VATCCM = 1161985
042100110401     C                   eval      VABCTR = 000
042200110323     C                   endif
042300110215     C*
042400110215     C                   movel     *blanks       wWHOIS           20
042500110215     C                   eval      wWHOIS = %trim(%subst(vindta:1:20))
042600110215     C                   if        %lookup(wWHOIS:skWHOIS:1) = 0
042700110215     C                   add       1             idxWHOIS
042800110215     C                   add       1             VABNCL
042900110215     C                   eval      skWHOIS(idxWHOIS) = wWHOIS
043000110310     C*
043100110310     C* ...VABPKB
043200110310     C                   EVAL      PiStr=%trim(%subst(vindta:371:8))
043300110310     C                   EXSR      CHKNUM
043400110310     C                   IF        PiNum=*on
043500110310     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
043600110310     C                   ADD(H)    PiVal         VABPKB
043700110310     C                   ELSE
043800110310     C                   ADD       1             errore
043900110310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044000110310     C                             + ' ' + 'VABPKB'
044100110310     C                   ENDIF
044200110310     C* ...VABVLB
044300110310     C                   EVAL      PiStr=%trim(%subst(vindta:363:8))
044400110310     C                   EXSR      CHKNUM
044500110310     C                   IF        PiNum=*on
044600110310     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
044700110310     C                   ADD(H)    PiVal         VABVLB
044800110310     C                   ELSE
044900110310     C                   ADD       1             errore
045000110310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100110310     C                             + ' ' + 'VABVLB'
045200110310     C                   ENDIF
045300110310     C*
045400110215     C                   endif
045500110215     C*
045600110215     C                   ENDSR
045700110215     C*----------------------------------------------------*
045800110215     C*  AZZERAMENTO CAMPI NN RIFERITI AL DESTINATARIO / RIFERIMENTI
045900110215     C*----------------------------------------------------*
046000110215     C     AZZDAT        BEGSR
046100110215     C*
046200110215     C                   EVAL      vabNCL = *zeros
046300110215     C                   EVAL      vabPKB = *zeros
046400110215     C                   EVAL      vabVLB = *zeros
046500110215     C                   EVAL      vabQFT = *zeros
046600110215     C                   EVAL      vabVMD = *zeros
046700110215     C                   EVAL      vabCAS = *zeros
046800110215     C                   EVAL      vabIAS = *zeros
046900110215     C*
047000110215     C                   ENDSR
047100110215
047200110215     C*----------------------------------------------------*
047300110215     C*  SCARICAMENTO BUFFER RECORDS VAB
047400110215     C*----------------------------------------------------*
047500110215     C     WRIVAB        BEGSR
047600110215     C*
047700110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
047800110215     C                   EVAL      VABCMR = 'ACCORPARE SEMPRE - ' +
047900110215     C                                      %char(datcor)+' '+%char(oracor)
048000110215     C                   EVAL      VABDCM = datcor
048100110215     C                   EVAL      VABDTS = datcor
048200110215     C                   EVAL      VABHMS = oracor
048300110215     C                   EVAL      VABCNT = 1
048400110215     C*
048500110215     C* Considerazioni sul contenuto di campi precedentemente valorizzati
048600110215     C                   IF        FlgCAS <> '0'
048700110215     C                   IF        VABCBO = '1'
048800110215     C                   EVAL      VABCBO = '4'
048900110215     C                   ELSE
049000110215     C                   EVAL      VABCBO = '6'
049100110215     C                   ENDIF
049200110215     C                   ENDIF
049300110215     C*
049400110215     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049500110215     C                   EXSR      CHKIMPDIV
049600110215     C*
049700110215     C* Ebbene...
049800110215     C                   ADD       1             §CTRMO
049900110215     C                   IF        errore <> *zeros
050000110215     C                   ADD       1             §CTRNO
050100110215     C                   EVAL      x_vinflg = '2'
050200110215     C                   ELSE
050300110215     C                   ADD       1             §CTROKVB
050400110215     C                   ENDIF
050500110215     C*
050600110215     C* Scarico il buffer della bolla "master"
050700110215     C                   EVAL      VABRMN=skSHIP(1)
050800110215     C                   EVAL      VABRMA=skDNS(1)
050900110215     C                   write     edivab00                                     => scarico il VAB
051000110215     C*
051100110215     C* Scarico il buffer dei "chi sono" della bolla "master"
051200110215     C                   exsr      wrivat
051300110215     C*
051400110215     C* Scarico il buffer delle bolle "slave"
051500110215     C                   exsr      wrislave
051600110215     C*
051700110215     C                   ENDSR
051800110215
051900010201     C*----------------------------------------------------*
052000110215     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
052100010201     C*----------------------------------------------------*
052200051209     C     WRIVAT        BEGSR
052300110215     C*
052400110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
052500110215     C                   EVAL      VATCMR = 'ACCORPARE SEMPRE - ' +
052600110215     C                                      %char(datcor)+' '+%char(oracor)
052700110215     C                   EVAL      VATCNT = 1
052800110215     C*
052900110215     C* Ciclo x tutti i "chi sono" della spedizione e li attacco alla
053000110215     C* spedizione "master"
053100110215     C                   z-add     1             i                 4 0
053200110215     C*
053300110215     C                   dow       i <= idxWHOIS
053400110215     C                   eval      VATTRC='E'
053500110215     C                   eval      VATNOT = skWHOIS(i)
053600110215     C                   write     EDIVAT00
053700110215     C                   ADD       1             §CTROKVB
053800110215     C*
053900110215     C                   add       1             i
054000110215     C                   enddo
054100010201     C*
054200010201     C                   ENDSR
054300020904
054400020904
054500110215     C*----------------------------------------------------*
054600110215     C*  CONSIDERAZIONI E SCRITTURA RECORD TESTATA BOLLE "SLAVE"
054700110215     C*----------------------------------------------------*
054800110215     C     WRISLAVE      BEGSR
054900110215     C*
055000110215     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
055100110215     C                   EVAL      VABCMR = 'ACCORPARE SEMPRE - ' +
055200110215     C                                      %char(datcor)+' '+%char(oracor)
055300110215     C                   EVAL      VABDCM = datcor
055400110215     C                   EVAL      VABDTS = datcor
055500110215     C                   EVAL      VABHMS = oracor
055600110215     C                   EVAL      VABCNT = 1
055700110215     C*
055800110215     C* Ciclo x tutte le DNs della spedizione e genero tante bolle "slave"
055900110215     C                   z-add     2             i                 4 0
056000110215     C*
056100110215     C                   dow       i <= idxDNS
056200110215     C* ...VABNSP => Stacco un numeratore da AZNUM
056300110215     C                   clear                   TRUL33DS
056400110215     C                   eval      I33OPE = *zeros
056500110215     C                   eval      I33CNU = 302
056600110215     C                   eval      I33NUM = 1
056700110215     C                   movel     TRUL33DS      KPJBU
056800110215     C                   call      'TRUL33R'
056900110215     C                   parm                    KPJBA
057000110215     C                   movel     KPJBU         TRUL33DS
057100110215     C                   if        O33ERR = *zeros
057200110215     C                   z-add     O33NRF        VABNSP
057300110215     C*
057400110215     C* Se tutto ok => procedo
057500110215     C                   exsr      azzdat
057600110215     C                   EVAL      VABRMN=skSHIP(1)
057700110215     C                   EVAL      VABRMA=skDNS(i)
057800110215     C                   EVAL      VABNOT=skDNS(i)
057900110215     C                   write     edivab00                                     => scarico il VAB
058000110215     C                   ADD       1             §CTROKVB
058100110215     C*
058200110215     C                   else
058300110215     C                   Z-ADD     1             errore
058400110215     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058500110215     C                             + ' ' + '"SLAVE"'
058600110215     C                   endif
058700110215     C*
058800110215     C                   add       1             i
058900110215     C                   enddo
059000110215     C*
059100110215     C                   ENDSR
059200110215
059300110215
059400010202     C*----------------------------------------------------*
059500110215     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
059600010202     C*----------------------------------------------------*
059700020305     C     PREVAT        BEGSR
059800010202     C*
059900110215     C* Compongo il nome del membro da dare al EDIVATWR
060000010202     C                   eval      parmbr = vlrhdl
060100010202     C                   movel     'M'           parmbr
060200051209     C                   eval      parccm = vlrksc
060300010202     C                   eval      paropz = '1'
060400010202     C* Effettuo la chiamata al CLLE preposto
060500110215     C                   call(e)   'TITVEVTC'
060600010202     C                   parm                    parccm
060700010202     C                   parm                    parmbr
060800010202     C                   parm                    paropz
060900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061000010202     C                   if        %error
061100010202     C                   movel     '1'           chkcall
061200010202     C                   else
061300010202     C                   movel     '0'           chkcall
061400010202     C                   endif
061500010202     C*
061600010202     C                   ENDSR
061700000801     C*----------------------------------------------------*
061800000801     C*  CONTROLLO NUMERICITA' CAMPI
061900000801     C*----------------------------------------------------*
062000000801     C     CHKNUM        BEGSR
062100000801     C*
062200000801     C                   call(e)   'ISNUMERIC'
062300000801     C                   PARM                    PiStr            30
062400000801     C                   PARM      ','           PiDecChr          1
062500000801     C                   PARM      *ZEROS        PiVal            30 9
062600000801     C                   PARM      '0'           PiInt             1
062700000801     C                   PARM      '0'           PiNum             1
062800000801     C                   IF        %error
062900000801     C                   EVAL      PiInt=*off
063000000801     C                   ENDIF
063100000801     C*
063200000801     C                   ENDSR
063300000801     C***
063400000801
063500011113
063600011113     C*----------------------------------------------------*
063700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063800011113     C*----------------------------------------------------*
063900011113     C     CHKIMPDIV     BEGSR
064000011113     C*
064100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
064200011113     C                   Z-ADD     *zeros        wrkDec            9 9
064300011113     C*
064400011113     C* Come prima cosa effettuo considerazioni sulla divisa
064500011113     C                   IF        vabIAS > *zeros
064600011113     C                   IF        vabVAS <> 'EUR'
064700011113     C                   EVAL      vabVAS =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   IF        vabCAS > *zeros
065200011113     C                   IF        vabVCA <> 'EUR'
065300011113     C                   EVAL      vabVCA =  'ITL'
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C                   IF        vabVMD > *zeros
065800020305     C                   IF        vabVAD <> 'EUR'
065900011113     C                   EVAL      vabVAD =  'ITL'
066000011113     C                   ENDIF
066100011113     C                   ENDIF
066200011113     C*
066300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066400011113     C                   Z-ADD     vabIAS        wrkDec
066500011113     C                   IF        wrkDec > *zeros
066600011113     C                   IF        vabVAS = 'ITL'
066700011113     C                   EVAL      vabIAS = *zeros
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
067200011113     C                   Z-ADD     vabCAS        wrkDec
067300011113     C                   IF        wrkDec > *zeros
067400011113     C                   IF        vabVCA = 'ITL'
067500011113     C                   EVAL      vabCAS = *zeros
067600011113     C                   ENDIF
067700011113     C                   ENDIF
067800011113     C*
067900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068000011113     C                   Z-ADD     vabVMD        wrkDec
068100011113     C                   IF        wrkDec > *zeros
068200011113     C                   IF        vabVAD = 'ITL'
068300011113     C                   EVAL      vabVMD = *zeros
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C                   ENDSR
068800011113     C***
068900011113
069000011113
069100000801
069200000801
069300990920      /TITLE Invio dei dati al punto operativo.
069400010202     C     invio         BEGSR
069500990920     C*
069600110215     C* 1° invio EDIVAT
069700010201     C                   reset                   dscmz
069800010201     C                   move      vlrpoi        cmzdst
069900110215     C                   eval      cmzfld = 'EDIVATWR'
070000010201     C                   eval      cmzmbd = vlrhdl
070100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070200021009     C***                if        prmfir = *blanks
070300110215     C                   eval      cmzfla = 'EDIVAT0F'
070400110215     C                   eval      cmzmba = 'EDIVAT0F'
070500021009     C***                else
070600021009     C***                eval      cmzfla = prmfir
070700021009     C***                eval      cmzmba = prmfir
070800021009     C***                endif
070900010201     C                   eval      cmznrr = *zeros
071000020305     C                   move      §ctrokvt      cmznrr
071100021018     C                   eval      cmzlba = vlrfl1
071200010201     C                   call(e)   'TIS711C'
071300010201     C                   parm                    dscmz
071400010201     C                   parm      *blanks       esito
071500010205     C                   if        %error
071600010205     C                             or cmzerr = '1'
071700010205     C                             or esito  = '1'
071800010205     C                   eval      wrkesito = '3'
071900010205     C                   else
072000010201     C*
072100110215     C* 2° invio EDIVAB
072200010201     C                   reset                   dscmz
072300010201     C                   move      vlrpoi        cmzdst
072400010201     C                   eval      cmzfld = vlrfou
072500010201     C                   eval      cmzmbd = vlrhdl
072600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072700021009     C***                if        prmfir = *blanks
072800110215     C                   eval      cmzfla = 'EDIVAB0F'
072900110215     C                   eval      cmzmba = 'EDIVAB0F'
073000021009     C***                else
073100021009     C***                eval      cmzfla = prmfir
073200021009     C***                eval      cmzmba = prmfir
073300021009     C***                endif
073400010201     C                   eval      cmznrr = *zeros
073500010201     C                   move      §ctrokvb      cmznrr
073600021018     C                   eval      cmzlba = vlrfl1
073700010201     C                   call(e)   'TIS711C'
073800010201     C                   parm                    dscmz
073900010201     C                   parm      *blanks       esito
074000010201     C                   if        %error
074100010201     C                             or cmzerr = '1'
074200010201     C                             or esito  = '1'
074300010201     C                   eval      wrkesito = '3'
074400010201     C                   endif
074500010205     C                   endif
074600990920     C*
074700000613     C                   ENDSR
074800000613     C***
074900070411
075000070411     C     *pssr         BEGSR
075100070411     C*
075200070411     C                   if        %open(tivin00r)
075300070411     C                   close     tivin00r
075400070411     C                   endif
075500110215     C                   if        %open(edivabwr)
075600110215     C                   close     edivabwr
075700070411     C                   endif
075800110215     C                   if        %open(edivatwr)
075900110215     C                   close     edivatwr
076000070411     C                   endif
076100070411     C*
076200070411     C* Effettuo la chiamata al CLLE preposto
076300110215     C                   call(e)   'TITVEVTC'
076400070411     C                   parm                    parccm
076500070411     C                   parm                    parmbr
076600070411     C                   parm      '2'           paropz
076700070411     C*
076800070411     C                   eval      wrkesito = '2'
076900070411     C*
077000070411     C                   seton                                        LR
077100070411     C*
077200070411     C                   ENDSR     '*CANCL'
077300070411     C***
077400990910
077500000613     C     *inzsr        BEGSR
077600990910     C*
077700990910     C     *entry        plist
077800990920     C                   parm                    tivlrds
077900990921     C                   parm      wrkesito      esito
078000000724     C                   parm                    prmlit
078100000710     C                   parm                    prmfir
078200000613     C*
078300000830     C* CALCOLA LA DATA CORRENTE
078400110215     C                   time                    wn14             14 0
078500110215     C                   movel     wn14          oracor            6 0          *ORA
078600110215     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078700110215     C                   eval      datcor = %dec(%date() : *ISO)
078800000830     C*
078900000613     C                   ENDSR
079000000613     C***
