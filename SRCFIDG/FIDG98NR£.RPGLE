000100140228      * --------------------------------------------------------------*
000200080228      *?INTERROGAZIONE ORGANIGRAMMA                                  ?*
000300080228      *---------------------------------------------------------------*
000400080228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500080228      *
000600080228      *?  A R C H I V I  ?--------------------------------------------*
000700080228      *
000800140313     fAZORG01L  uf   e           k disk    usropn
000900080228     fTABEL00F  if   e           k disk
001000080228      *
001100140228     ffidg98nd  cf   e             workstn
001200080228      *
001300080228      *?  C O S T A N T I  ?------------------------------------------*
001400080228      *
001500080228      *?  S C H I E R E  ?--------------------------------------------*
001600080228      * - Messaggi di errore
001700140305     d $Msg            s             78    dim(05)  ctdata  perrcd(1)
001800140620     d cmd             s             80    dim(4)  ctdata perrcd(1)
001900080228      *
002000080228      *?  S T R U T T U R E   D A T I  ?------------------------------*
002100080228      *
002200080228      * Parametri ricevuti
002300080228     d KPJBA         e ds
002400140305     d Og146         e ds                  inz
002500140305     d Og147         e ds                  inz
002600140228     d Og148         e ds                  inz
002700080228      *
002800080228      * - Parametri x Controllo profilo utenti
002900080228     d TIBS34ds      e ds                  inz
003000080228      * - Ds di riferimento al file esterno AZUTE00F
003100080228     d AZUTEds       e ds                  extname(AZUTE00F)
003200080228      * - Ds per dati organigramma
003300080228     d dDatiUte      e ds
003400080228      * - Abilitazioni utenti aziendali
003500080228     d dUte01        e ds
003600140619     d tibs55ds      e ds                  inz
003700140613     d fnlv55ds      e ds                  inz
003800130830
003900080228      * - Controllo/Inversione date
004000080228     d WLBdat          ds                  inz
004100080228     d  G02dat                 1      8  0 inz
004200080228     d  G02inv                 9     16  0 inz
004300080228     d  G02err                17     17    inz('3')
004400080228     d  G02tgi                18     22  0 inz
004500080228      *
004600080228      *?  V A R I A B I L I  ?----------------------------------------*
004700140228      * - Parametri per TNSD24R
004800140228     d §§Cod           s              3    inz
004900140306     d §§tip           s                   like(ORGfag)  inz
005000140228     d §§Des           s             25    inz
005100140228     d §§Pra           s              3    inz
005200080228      *
005300080228      * - Flag booleani
005400080228      *
005500080228      * - Indici di schiera
005600080228     d xa              s              3  0 inz
005700080228      *  - Comodo
005800080228     d §Fil            s                   like(ORGfil)  inz
005900080228     d wFil            s                   like(ORGfil)  inz
006000130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
006100130830     d klna            s              3  0 inz
006200140313     d command         s             80
006300140313     d lung            s             15  5 inz(80)
006400140620      *
006500140620     D Corg            C                   CONST('/AZORG01L)')
006600140620     D Carb            C                   CONST('/fnarb00f) SECURE(*YES)')
006700140620     D Carg            C                   CONST('/fnarg00f) SECURE(*YES)')
006800140620     D Cdst            C                   CONST('/fidst01l) SECURE(*YES)')
006900080228      *
007000080228      *?  M A I N - L I N E  ?----------------------------------------*
007100080228      *
007200080228     c     *Entry        plist
007300080228     c                   parm                    KPJBA
007400140619      * reperimento libreria sistema informativo
007500140619     C                   CLEAR                   tibs55ds
007600140619     c                   movel     'L'           i50tla
007700140619     c                   Movel     046           I50apo
007800140619     C                   CALL      'TIBS55R'                              01
007900140619     C                   PARM                    tibs55ds
008000140619     C                   movel     o50alg        libsede          10
008100140619     c
008200140619     C                   CLEAR                   tibs55ds
008300140619     c                   movel     'L'           i50tla
008400140619     c                   Movel     001           I50apo
008500140619     C                   CALL      'TIBS55R'
008600140619     C                   PARM                    tibs55ds
008700140619     C                   movel     o50alg        libfiliale       10
008800140619     C                   movel     o50ala        libfilfil        10
008900140619     C
009000140313      * ovrdbf azorg01l di filiale
009100140313     c                   clear                   command
009200140619     c                   movea     cmd(1)        command
009300140619     c                   eval      %subst(command:30 :10) = libfiliale
009400140620     c                   eval      command=%trim(command) + Corg
009500140313     c                   call      'QCMDEXC'
009600140313     c                   parm                    command
009700140313     c                   parm                    lung
009800140313     c                   open      azorg01l
009900131021      *
010000140313      *
010100140305     C                   TIME                    W0140            14 0
010200140305     C                   MOVE      W0140         UDATE8            8 0
010300140305     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
010400140305     C*
010500140305     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
010600140305     C                   MOVEL     *BLANK        G02ERR
010700140305     C                   CALL      'XSRDA8'
010800140305     C                   PARM                    WLBDAT
010900140305     C                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
011000080228      *
011100080228      * Reperisco dati job
011200080228     c                   exsr      DatiJob
011300080228      *
011400080228      * 88 on - organigramma di sede
011500080228     c                   eval      *in88 = (%subst(KPJBU : 1 : 4) = 'SEDE')
011600080228     c                   eval      TBLkut = 1
011700080228      *
011800080228      *---------------------------------------------------------------------
011900030430      *
012000000000     C     INIZIO        TAG
012100080228     C                   movel     *blanks       VFil
012200030430      *
012300000000     C     VID1          TAG
012400140306     c                   setoff                                       808182
012500140613     c                   setoff                                       838440
012600140228     C                   EXFMT     video1
012700000000     C   KC              GOTO      FINE
012800130904      *
012900030430      *
013000030430      * Gestione VIDEO 1
013100030430      *
013200000000     C     VFIL          IFEQ      *ZERO
013300000000     C                   SETON                                        50
013400000000     C                   GOTO      VID1
013500000000     C                   END
013600000000     C*
013700080228     C     '?'           scan      VFil                                   21
013800000000     C   21              DO
013900080228     C   88              MOVE      '§'           §§Tip
014000971203     C                   CALL      'TNSD24R'
014100080228     C                   PARM                    §§Cod
014200080228     C                   PARM                    §§Tip
014300080228     C                   PARM                    §§DES
014400080228     C                   PARM                    §§PRA
014500080228     C                   MOVEL     §§COD         VFIL
014600000000     C                   GOTO      VID1
014700080228     C                   ENDdo
014800000000     C** CONTROLLO CHE IL CODICE FILIALE IMMESSO SIA
014900000000     C** NUMERICO
015000040922     C                   TESTN                   VFIL                 36
015100040922     c                   move      vfil          w001a             1
015200040922     c
015300040922     c                   if        not *in36 or w001a<'0'
015400000000     C                   SETON                                        50
015500000000     C                   GOTO      VID1
015600000000     C                   END
015700000000     C*
015800080228     C                   MOVEL     VFIL          §Fil
015900140305     C     §Fil          CHAIN     AZORG                              36
016000020617     C     *IN36         IFEQ      *ON
016100000000     C                   SETON                                        50
016200000000     C                   GOTO      VID1
016300040922     C                   endif
016400140305     C                   MOVEL     ORGDE6        OG146
016500140305     C                   MOVEL     ORGDE7        OG147
016600140228     C                   MOVEL     ORGDE8        OG148
016700140305     C                   EXSR      pulVid2
016800140305     C                   EXSR      AggVid2
016900040303     c
017000080228      * Descrizione p.o.
017100140305     c                   eval      VDFil = orgdes
017200140305     c     sopra         tag
017300140305     c                   setoff                                       90
017400130423
017500140305     c                   do        *hival
017600140228     C                   EXFMT     video2
017700140305     c                   setoff                                       28
017800080228      * Fine lavoro
017900080228     C   KC              GOTO      INIZIO
018000070828      * Video precedente
018100140228     C   KL              GOTO      VID1
018200140305     c                   exsr      contr2
018300030430      *
018400140305     C   90              GOTO      sopra
018500140313     c                   if        not *in80 or not *in81 or not *in82 or
018600140313     c                             not *in83 or  not *in84
018700140305     c   kf              exsr      aggiorna
018800140306     C   kf              GOTO      VID1
018900140313     c                   endif
019000140305     c                   enddo
019100000000     C     FINE          TAG
019200140616      *cancella eventuali ovrdbf per chiamate secondarie
019300140616     c                   clear                   command
019400140616     c                   eval      command = 'DLTOVR FILE(*ALL) LVL(*JOB)'
019500140616     c                   call      'QCMDEXC'
019600140616     c                   parm                    command
019700140616     c                   parm                    lung
019800030430      *
019900000000     C                   SETON                                        LR
020000080228      *
020100080228      *---------------------------------------------------------------*
020200080228      * Reperimento Dati del job (Utente/Operativi)                   *
020300080228      *---------------------------------------------------------------*
020400080228     c     DatiJob       BEGSR
020500080228      *
020600080228     c     *dtaara       define    §azute        azuteds
020700080228     c     *dtaara       define    §datiute      ddatiute
020800080228      *
020900080228     c                   in(E)     *dtaara
021000080228     c                   IF        %ERROR or RSUT = *blanks
021100080228     c                   clear                   Tibs34Ds
021200080228     c                   call      'TIBS34R'
021300080228     c                   parm                    Tibs34Ds
021400080228     c                   in        *dtaara
021500080228     c                   ENDIF
021600080228      *
021700080228     c                   eval      dUTE01 = UTEfaf
021800080228      *
021900080228     c                   ENDSR
022000140613      *------------------------------------------------------
022100140613      * PartCons
022200140613      *------------------------------------------------------
022300140613     C     partCons      BEGSR
022400140613     c*ovrdbf per fnarb per chiamato
022500140613     c                   clear                   command
022600140620     c                   movea     cmd(2)        command
022700140619     c                   eval      %subst(command:30 :10) = libfilfil
022800140620     c                   eval      command=%trim(command) + Carb
022900140613     c                   call      'QCMDEXC'
023000140613     c                   parm                    command
023100140613     c                   parm                    lung
023200140613     c*ovrdbf per fnarg per chiamato
023300140613     c                   clear                   command
023400140620     c                   movea     cmd(3)        command
023500140619     c                   eval      %subst(command:30 :10) = libfilfil
023600140620     c                   eval      command=%trim(command) + Carg
023700140613     c                   call      'QCMDEXC'
023800140613     c                   parm                    command
023900140613     c                   parm                    lung
024000140613     c*
024100140616     c                   call      'FIDG99R'
024200140613     c                   parm                    vfil
024300140613     c                   ENDSR
024400140613      *------------------------------------------------------
024500140613      * PartRit
024600140613      *------------------------------------------------------
024700140613     C     partRit       BEGSR
024800140613     c                   call      'FIDG94R'
024900140613     c                   parm                    vfil
025000140613     c*ovrdbf per fnarb per chiamato
025100140613     c                   clear                   command
025200140620     c                   movea     cmd(4)        command
025300140619     c                   eval      %subst(command:30 :10) = libfilfil
025400140620     c                   eval      command=%trim(command) + Cdst
025500140613     c                   call      'QCMDEXC'
025600140613     c                   parm                    command
025700140613     c                   parm                    lung
025800140613     c                   call      'TNSY01R'
025900140613     c                   parm                    vfil
026000140613     c                   ENDSR
026100070828      *------------------------------------------------------
026200140305      * Pulizia VIDEO 2
026300070828      *------------------------------------------------------
026400140305     C     PULVID2       BEGSR
026500070828      *
026600140305     C                   movel     *blanks       Vgeot
026700070828     C                   z-add     0             Vdda
026800070828     c                   z-add     0             vddao
026900070828     C                   movel     *blanks       Vtlls
027000070828     c                   movel     *blanks       vcgio
027100070828      *
027200070828     C                   ENDSR
027300140305      *------------------------------------------------------
027400140305      * aggiorna
027500140305      *------------------------------------------------------
027600140305     C     aggiorna      BEGSR
027700140313      * chiudo il file aperto
027800140313     c                   if        %open(azorg01l)
027900140313     c                   close     azorg01l
028000140313     c                   endif
028100140313
028200140313      * ovrdbf azorg01l di filiale
028300140313     c                   clear                   command
028400140619     c                   movea     cmd(1)        command
028500140619     c                   eval      %subst(command:30 :10) = libfiliale
028600140620     c                   eval      command=%trim(command) + Corg
028700140313     c                   call      'QCMDEXC'
028800140313     c                   parm                    command
028900140313     c                   parm                    lung
029000140313     c                   open      azorg01l
029100140313
029200140313      * aggiorno organigramma di filiale
029300140313     c     §fil          chain     azorg01l
029400140313     c                   if        %found(azorg01l)
029500140613     C                   MOVEL     ORGDE6        OG146
029600140613     C                   MOVEL     ORGDE7        OG147
029700140613     C                   MOVEL     ORGDE8        OG148
029800140313     C                   move      Vgeot         §oggeot
029900140613      *partenza consegne
030000140613     c                   if        §ogdda = *blank and wvdda <> 0
030100140613     c                   exsr      partCons
030200140613     c                   endif
030300140313     C                   move      wvdda         §ogdda
030400140613     c                   if        §ogddao = *blank and wvddao <> 0
030500140613     c                   exsr      partRit
030600140613     c                   endif
030700140313     c                   move      wvddao        §ogddao
030800140313     C                   movel     Vtlls         §ogtlls
030900140313     c                   movel     vcgio         §ogcgio
031000140313     C                   MOVEL(p)  og146         ORGDE6
031100140313     C                   MOVEL(p)  og147         ORGDE7
031200140313     C                   MOVEL(p)  og148         ORGDE8
031300140313     c                   update    azorg
031400140313     c                   endif
031500140313
031600140313      * chiudo il file aperto
031700140313     c                   if        %open(azorg01l)
031800140313     c                   close     azorg01l
031900140313     c                   endif
032000140313
032100140313      * ovrdbf azorg01l di sede
032200140313     c                   clear                   command
032300140620     c                   movea     cmd(1)        command
032400140619     c                   eval      %subst(command:30 :10) = libsede
032500140620     c                   eval      command=%trim(command) + Corg
032600140313     c                   call      'QCMDEXC'
032700140313     c                   parm                    command
032800140313     c                   parm                    lung
032900140313     c                   open      azorg01l
033000140313
033100140313      * imposto subito il flag in organigramma di filiale
033200140313     c     §fil          chain     azorg01l
033300140313     c                   if        %found(azorg01l)
033400140613     C                   MOVEL     ORGDE6        OG146
033500140613     C                   MOVEL     ORGDE7        OG147
033600140613     C                   MOVEL     ORGDE8        OG148
033700140313     C                   move      Vgeot         §oggeot
033800140313     C                   move      wvdda         §ogdda
033900140313     c                   move      wvddao        §ogddao
034000140313     C                   movel     Vtlls         §ogtlls
034100140313     c                   movel     vcgio         §ogcgio
034200140313     C                   MOVEL(p)  og146         ORGDE6
034300140313     C                   MOVEL(p)  og147         ORGDE7
034400140313     C                   MOVEL(p)  og148         ORGDE8
034500140313     c                   update    azorg
034600140313     c                   endif
034700140305      *
034800140305     C                   ENDSR
034900070828      *-----------------------------------------------------
035000140305      * Riempimento  video 2
035100070828      *-----------------------------------------------------
035200140305     C     AGGVID2       BEGSR
035300070828      *
035400070828      * flag avviamento gestione telefonate per GEO
035500070828     c                   movel     §oggeot       vgeot
035600140306     c                   if        vgeot = 'S'
035700140313     c                   seton                                        81
035800140306     c                   endif
035900070828     c* data inizio distinta di consegne automatica
036000070828     c                   if        §ogdda<>*blanks
036100140313     c                   seton                                        82
036200070828     C                   CLEAR                   WLBDAT
036300070828     C                   MOVEL     '3'           G02ERR
036400070828     C                   move      §ogdda        G02INV
036500070828     C                   CALL      'XSRDA8'
036600070828     C                   PARM                    WLBDAT
036700070828     C                   Z-ADD     G02DAT        Vdda
036800070828     c                   endif
036900070828      * Procedura GIRI su ORM
037000070828     c                   move      §ogcgio       vcgio
037100140306     c                   if        vcgio = 'S'
037200140313     c                   seton                                        83
037300140306     c                   endif
037400070828      * Data inizio distinta ORM automatica
037500070828     c                   if        §ogddao <> *blanks
037600140313     c                   seton                                        84
037700070828     c                   clear                   wlbdat
037800070828     c                   eval      g02err = '3'
037900070828     c                   move      §ogddao       g02inv
038000070828     c                   call      'XSRDA8'
038100070828     c                   parm                    wlbdat
038200070828     c                   z-add     g02dat        vddao
038300070828     c                   endif
038400070828      * TELLUS
038500070828     C                   MOVE      §ogtlls       Vtlls
038600140306     c                   if        vtlls = 'S'
038700140313     c                   seton                                        80
038800140306     c                   endif
038900070828     C                   ENDSR
039000140305      *-----------------------------------------------------
039100140305      * controlli    video 2
039200140305      *-----------------------------------------------------
039300140305     C     Contr2        begsr
039400140305      *
039500140305     C                   if        §ogtlls = *blank and
039600140305     c                             (vtlls <> *blank and vtlls <> 'D')
039700140305     c                   seton                                        2890
039800140305     c                   movel     $msg(2)       v1dmsg
039900140305     c                   leavesr
040000140305     c                   endif
040100140305      * flag avviamento gestione telefonate per GEO
040200140305     c                   if        vgeot = 'S' and vtlls = ' '
040300140305     c                   seton                                        2890
040400140305     c                   movel     $msg(3)       v1dmsg
040500140305     c                   leavesr
040600140305     c                   endif
040700140305     c* data inizio distinta di consegne automatica
040800140305     c                   if        vdda > 0
040900140305     C                   CLEAR                   WLBDAT
041000140305     C                   MOVEL     '1'           G02ERR
041100140305     C                   move      vdda          G02dat
041200140305     C                   CALL      'XSRDA8'
041300140305     C                   PARM                    WLBDAT
041400140305    1C     G02ERR        IFEQ      '1'
041500140305     c                   eval      v1dmsg = 'Data errata'
041600140305     C                   SETON                                        3328
041700140305     C                   leavesr
041800140305     c                   endif
041900140305     c                   move      g02inv        wvdda             8 0
042000140305     c                   move      g02inv        vddaa             8
042100140616     c                   move      g02dat        vdda
042200140305    1C                   IF        g02inv <= DATEU8 and
042300140305     C                             §ogdda <>  Vddaa
042400140305     c                   eval      v1dmsg = $msg(4)
042500140305     C                   SETON                                        3328
042600140305     C                   leavesr
042700140305    1C                   ENDIF
042800140305     c                   endif
042900140613      *test per vedere se la filiale è gestita da altra
043000140613     c                   if        not *in40
043100140613     c                   Clear                   fnlv55ds
043200140613     c                   Eval      d55tpt = '6'
043300140613     c                   Move      vfil          d55lin
043400140613     c                   Eval      d55drf = wvdda
043500140613     c                   Call      'FNLV55R'
043600140613     c                   Parm                    Fnlv55ds
043700140613     c                   If        d55err = *Blanks
043800140613     c                   eval      v1dmsg = 'La filiale alla data risulta in £6'
043900140613     C                   SETON                                        4028
044000140613     c                   leavesr
044100140613     c                   EndIf
044200140613     c                   EndIf
044300140305      * Procedura GIRI su ORM
044400140305     c                   if        vcgio = 'S' and vtlls = ' '
044500140305     c                   seton                                        2890
044600140305     c                   movel     $msg(5)       v1dmsg
044700140305     c                   leavesr
044800140305     c                   endif
044900140305      * Data inizio distinta ORM automatica
045000140305     c                   if        vddao > 0
045100140305     C                   CLEAR                   WLBDAT
045200140305     C                   MOVEL     '1'           G02ERR
045300140305     C                   move      vddao         G02dat
045400140305     C                   CALL      'XSRDA8'
045500140305     C                   PARM                    WLBDAT
045600140305    1C     G02ERR        IFEQ      '1'
045700140305     c                   eval      v1dmsg = 'Data errata'
045800140305     C                   SETON                                        3228
045900140305     C                   leavesr
046000140305     c                   endif
046100140305     c                   move      g02inv        wvddao            8 0
046200140305     c                   move      g02inv        vddaoa            8
046300140616     c                   move      g02dat        vddao
046400140305    1C                   IF        g02inv <= DATEU8 and
046500140305     C                             §ogddao <>  Vddaoa
046600140305     c                   eval      v1dmsg = $msg(4)
046700140305     C                   SETON                                        3228
046800140305     C                   leavesr
046900140305    1C                   ENDIF
047000140305     c                   endif
047100140305      *
047200140305     C                   ENDSR
047300130830      *
047400080228** - $Msg -------------------------------------------------------------------*
047500080228Utente NON abilitato - Premere "Invio" per tornare a menù                      01
047600140305Se precedente valore " " si può inserire solo D                                02
047700140305Per attivare telefonate in consegna deve essere già attivo lo scambio dati     03
047800140305La data deve essere maggiore di oggi                                           04
047900140305Per attivare Gestione ORM deve essere già attivo lo scambio dati               05
048000140313** CMD   (Lunga 80)                                                            *
048100140620OVRDBF FILE(AZORG01L) TOFILE(                                                     1
048200140620OVRDBF FILE(fnarb00f) TOFILE(                                                     2
048300140620OVRDBF FILE(fnarg00f) TOFILE(                                                     3
048400140620OVRDBF FILE(fidst01l) TOFILE(                                                     4
