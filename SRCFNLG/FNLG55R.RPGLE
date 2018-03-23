000100071015     H* ACTGRP=*CALLER
000200071015     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300071015     ffnarbx3l  uf   e           k disk
000400071015     FFnarb01l  IF   E           K DISK    rename(fnarb000:fnarb) prefix(a)
000500071015     FFnlbl02l  IF   E           K DISK
000600071015     FFnblp01l  IF   E           K DISK
000700071015     Ftabel00F  IF   E           K DISK
000800071015     FWFGCS00F  O    E             DISK    USROPN
000900071015     FWFGCS01L  IF   E           K DISK    RENAME(WFGCS000:WFGCS1) USROPN
001000071015     Ffnart01L  IF   E           K DISK
001100071015     Ffnblt01L  IF   E           K DISK
001200071015     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300071015     D FIL£6           S              3  0 DIM(30)
001400071015     D §FLE            S              3  0
001500140728     D spuntato        S              8
001600071015     D TCUDS           DS
001700071015     D  F1                     1      1
001800071015     D  F3                     3      3
001900071015     D  F2                     2      2
002000071015     D  F4                     4      4
002100071015     D  F56                    5      6
002200071015     D WLBDA8          DS
002300071015     D  G08DAT                 1      8  0
002400071015     D  G08INV                 9     16  0
002500071015     D  G08ERR                17     17
002600071015     D  G08TGI                18     22  0
002700071015      *
002800071015     d dar5gen       e ds
002900071015     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003000071015     D  TCU                  398    697
003100071015     D                                     DIM(50)
003200071015     D  KCU                  698    847P 0
003300071015     D                                     DIM(50)
003400071015     D                                     PACKEVEN
003500071015     D  DTU                  848    895P 0
003600071015     D                                     DIM(12)
003700071015     D                                     PACKEVEN
003800071015     d dskiere         ds
003900071015     d  sk1                    1    140  0 dim(20)
004000071015     d  sk2                  141    280  0 dim(20)
004100071015     d  sk3                  281    420  0 dim(20)
004200071015     d  sk4                  421    560  0 dim(20)
004300071015     d  sk5                  561    700  0 dim(20)
004400071015     d  skt                    1    700  0 dim(100)
004500071017     d dskieref        ds
004600071017     d  sk1f                   1    140  0 dim(20)
004700071017     d  sk2f                 141    280  0 dim(20)
004800071017     d  sk3f                 281    420  0 dim(20)
004900071017     d  sk4f                 421    560  0 dim(20)
005000071017     d  sk5f                 561    700  0 dim(20)
005100071018     d  sktf                   1    700  0 dim(100)
005200071015
005300071015     D DSCMZ         E DS
005400071015     D CNCR80        E DS
005500071015     D KPJBA         E DS
005600071015     d trul06ds      e ds
005700071015     d  lin                    1     90  0 dim(30)
005800071019     D FNLG54ds      E DS
005900071015      *
006000071015     D                SDS
006100071015     D  §PGM                   1     10
006200071015     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006300071015     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006400071015     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006500071015     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006600071015     d comman          s             80
006700071015     d lenght          s             15  5 inz(80)
006800071015      *----------------------------------------------------------------
006900071015     C                   DO        30            I                 2 0
007000071015     c                   if        fil£6(i) = 0
007100071015     c                   leave
007200071015     c                   endif
007300071015     C                   Z-ADD     fil£6(i)      FLE               3 0
007400071026      * lettura file spia
007500071015      *
007600071015     c     karbx         setll     fnarbx3l
007700071015     c                   do        *hival
007800071015     c     karbx         reade     fnarbx3l
007900071015     c                   if        %eof(fnarbx3l)
008000071015     c                   leave
008100071015     c                   end
008200071015      *
008300071015      * considero solo le tipologie R=resi e D=dirottamenti
008400071015     c                   if        arbtpl <> 'R' and arbtpl <> 'D'
008500071015     c                   iter
008600071015     c                   end
008700071107      * solo resi
008800071107     c                   if        d54tst = '1' and arbtpl <> 'R'
008900071107     c                   iter
009000071107     c                   endif
009100071107      * solo dirottamenti
009200071107     c                   if        d54tst = '2' and arbtpl <> 'D'
009300071107     c                   iter
009400071107     c                   endif
009500071015      *
009600071015     C* PRELEVO DATI BOLLA
009700071015     C     Karb          chain     fnarb01l                           30
009800071017     C     Karb          chain     fnlbl02l                           31
009900071015     C  n31Kblp          chain     fnblp01l                           31
010000071015      *scrittura wf con bolle da stampare successivamente
010100071015     c                   exsr      scriviwf
010200071015     c
010300071015     C* AGGIORNO DATA DI STAMPA
010400071019     C     d54ris        ifeq      *blank
010500071015     C                   move      amgud8        arbdst
010600071015     C                   EXCEPT    agg5
010700071019     c*                  if        gescomm = 'S'
010800071019     c*                  commit
010900071019     c*                  endif
011000071015
011100071015     C                   END
011200071015     C*
011300071015     c                   enddo
011400071015
011500071015     c                   enddo
011600071015      * eliminazione file di work
011700071015     c                   close     wfgcs00f
011800071015      * rilegge wf e produce il tabulato
011900071015     c                   exsr      stampa
012000071015      *
012100071015     c                   Movel(p)  cmd1(1)       comman
012200071015     c                   eval      lenght = %len(%trim(comman))
012300071015     c                   Call      'QCMDEXC'                            97
012400071015     c                   Parm                    comman
012500071015     c                   Parm                    lenght
012600071015     c                   close     wfgcs01l
012700071015     c                   Movel(p)  cmd3(1)       comman
012800071015     c                   eval      lenght = %len(%trim(comman))
012900071015     c                   Call      'QCMDEXC'                            97
013000071015     c                   Parm                    comman
013100071015     c                   Parm                    lenght
013200071015     c                   seton                                        lr
013300071015      *_________________________________________________________
013400071015     c     stampa        begsr
013500071015      *_________________________________________________________
013600071015      * legge la v.l. del w.f. per la stampa
013700071015     c     *loval        setll     wfgcs01l
013800071015     c                   do        *hival
013900071015     c                   read      wfgcs01l                               98
014000071015     c   98              leave
014100071015      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
014200071015      * bolla mamma
014300140717     c                   clear                   skt
014400071015     c                   if        gcsfns = 'S'
014500071015     C     Kart          setll     Fnart01L
014600071015     c                   z-add     0             ncl               3 0
014700071015     c                   do        *hival
014800071015     C     Kart          reade     Fnart01L
014900071015     c                   if        %eof(fnart01l) or ncl = 100
015000071015     c                   leave
015100071015     c                   endif
015200071015     c                   add       1             ncl
015300071015     c                   z-add     artnsc        skt(ncl)
015400071015     c                   enddo
015500071015     c                   endif
015600071015     c
015700071015      * bolla figlia
015800071018     c                   clear                   sktf
015900140728     c                   clear                   spuntato
016000140728     c                   clear                   spu               3 0
016100140728     c                   clear                   nospu             3 0
016200071015     C     Kblt          setll     Fnblt01L
016300071015     c                   z-add     0             ncl               3 0
016400071015     c                   do        *hival
016500071015     C     Kblt          reade     Fnblt01L
016600071015     c                   if        %eof(fnblt01l) or ncl = 100
016700140728     c                   select
016800140728     c                   when      spu >= 1 and nospu = 0
016900140728     c                   eval      spuntato = 'Totale'
017000140728     c                   when      spu >= 1 and nospu >= 1
017100140728     c                   eval      spuntato = 'Parziale'
017200140728     c                   other
017300140728     c                   clear                   spuntato
017400140728     c                   endsl
017500071015     c                   leave
017600071015     c                   endif
017700140717     c* se presente la data entrata vuol dire
017800140717     c* che è già stato etichettato il pacco con la nuova
017900140717     c* etichetta della figlia
018000140717     c                   if        bltdse <> 0
018100140728     c                   add       1             spu
018200140728     c                   else
018300140728     c                   add       1             nospu
018400140717     c                   end
018500140717     c*
018600140717     c                   if        gcsfnsf = 'S'
018700071015     c                   add       1             ncl
018800071018     c                   z-add     bltnsc        sktf(ncl)
018900140717     c                   endif
019000071015     c                   enddo
019100071015     c
019200071015      *
019300071015     C     GCsDIS        IFNE      SAVDIS
019400071015     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
019500071015     C                   MOVEL     '2D'          COD
019600071015     C                   MOVEL     *BLANKS       KEY
019700071015     C                   MOVEL     GCsDIS        KEY
019800071015     C     KTAB          CHAIN     TABEL                              50
019900071015     C  N50              MOVEL     TBLUNI        DESCR            20
020000071015     C   50              MOVEL     *ALL'?'       DESCR
020100071015     C                   EXCEPT    TESTA
020200071019     C     d54RIS        IFEQ      'R'
020300071015     C                   EXCEPT    TESTAR
020400071015     C                   END
020500071015     C                   EXCEPT    TESTAT
020600071015     C                   SETON                                        99
020700071015     C                   MOVE      GCsDIS        SAVDIS            1
020800071015     C                   END
020900071015      *
021000071015     C  N99
021100071015     COR OF              DO
021200071015     C                   EXCEPT    TESTA
021300071019     C     d54RIS        IFEQ      'R'
021400071015     C                   EXCEPT    TESTAR
021500071015     C                   END
021600071015     C                   EXCEPT    TESTAT
021700071015     C                   END
021800071015     C  N99              SETON                                        99
021900071015      * prima riga dettaglio bolla mamma
022000071015     C                   EXCEPT    RIGA
022100071015      * stampa dettaglio dei segnacolli se non sequenziali
022200071015     c                   if        skt(1) > 0
022300071015     c                   except    segn1
022400071015     c                   endif
022500071015     c                   if        skt(21) > 0
022600071015     c                   except    segn2
022700071015     c                   endif
022800071015     c                   if        skt(41) > 0
022900071015     c                   except    segn3
023000071015     c                   endif
023100071015     c                   if        skt(61) > 0
023200071015     c                   except    segn4
023300071015     c                   endif
023400071015     c                   if        skt(81) > 0
023500071015     c                   except    segn5
023600071015     c                   endif
023700071015      * seconda riga dettaglio bolla figlia
023800071015      * testa se il campo spedizione è pieno per capire se è stato
023900071015      * agganciato il legame altrimenti stampa anomalia
024000071015     c                   if        gcsnspf > 0
024100071015     C                   EXCEPT    RIGAf
024200071015      * stampa dettaglio dei segnacolli se non sequenziali
024300071018     c                   if        sktf(1) > 0
024400071015     c                   except    segn1f
024500071015     c                   endif
024600071018     c                   if        sktf(21) > 0
024700071015     c                   except    segn2f
024800071015     c                   endif
024900071018     c                   if        sktf(41) > 0
025000071015     c                   except    segn3f
025100071015     c                   endif
025200071018     c                   if        sktf(61) > 0
025300071015     c                   except    segn4f
025400071015     c                   endif
025500071018     c                   if        sktf(81) > 0
025600071015     c                   except    segn5f
025700071015     c                   endif
025800071026      * separazione
025900071026     C                   EXCEPT    separa
026000071015     c                   else
026100071015     C                   EXCEPT    errRIGAf
026200071015     c                   endif
026300071015      *
026400071015     c                   enddo
026500071015     c                   endsr
026600071015      *_________________________________________________________
026700071015     c     scriviwf      begsr
026800071015      *_________________________________________________________
026900071015     c                   clear                   wfgcs000
027000071015     c                   move      arbtpl        GCSdis
027100071015     c                   z-add     aarbaas       GCSaas
027200071015     c                   z-add     aarblnp       GCSlnp
027300071015     c                   z-add     aarbnrs       GCSnrs
027400071015     c                   z-add     aarbnsp       GCSnsp
027500071015     c                   z-add     aarblna       GCSlna
027600071015     c                   move      aarbrsm       GCSrsm
027700071015     c                   move      aarbrsd       GCSrsd
027800071015     c                   z-add     aarbznc       GCSznc
027900071026     c                   z-add     aarbfls       GCSfls
028000071026     c                   z-add     aarbncd       GCSncd
028100071015     c                   z-add     aarbnca       GCSnca
028200071015     c                   move      aarbfns       GCSfns
028300071015     c                   z-add     aarbncl       GCSncl
028400071015     c                   z-add     aarbpkf       GCSpkf
028500071015     c                   z-add     aarbvlf       GCSvlf
028600071015      *figlia se non trova legame o blp non scrive nulla
028700071015     c                   if        not *in31
028800071015     c                   z-add     blpaas        GCSaasf
028900071015     c                   z-add     blplnp        GCSlnpf
029000071015     c                   z-add     blpnrs        GCSnrsf
029100071015     c                   z-add     blpnsp        GCSnspf
029200071015     c                   z-add     blplna        GCSlnaf
029300071015     c                   move      blprsm        GCSrsmf
029400071015     c                   move      blprsd        GCSrsdf
029500071015     c                   z-add     blpznc        GCSzncf
029600071026     c                   z-add     blpfls        GCSflsf
029700071015     c                   z-add     blpncd        GCSncdf
029800071015     c                   z-add     blpnca        GCSncaf
029900071015     c                   move      blpfns        GCSfnsf
030000071015     c                   z-add     blpncl        GCSnclf
030100071015     c                   z-add     blppkf        GCSpkff
030200071015     c                   z-add     blpvlf        GCSvlff
030300071015     c                   endif
030400071015      *
030500071011     c                   write     wfgcs000
030600071011     c                   endsr
030700071026     c
030800071015     C     *INZSR        BEGSR
030900071015     C*---------------------------------------------------------------*
031000071015     C     *ENTRY        PLIST
031100071015     C                   PARM                    KPJBA
031200071019     C                   MOVEL     KPJBU         FNLG54ds
031300071015      *
031400071019     c                   move      d54comm       gescomm           1
031500071015      *
031600071015     C                   Z-ADD     1             CODUT
031700071015     C                   CALL      'X§PARUT'
031800071015     C                   PARM                    UTEDSE
031900071015     C                   MOVEL     REC80         CNCR80
032000071015     C                   Z-ADD     0             SAVLNP            3 0
032100071015      *
032200071015     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
032300071015     C     SimTpP        COMP      '2'                                    02
032400071015     C                   Z-ADD     SIMPOU        §FLE
032500071015      *
032600071015     C                   TIME                    WHHUDT           14 0
032700071015     C                   MOVEL     WHHUDT        ORA               6 0
032800071015     C                   MOVE      WHHUDT        GMAUD8            8 0
032900071015     C* GIRO UDATE
033000071015     C                   MOVE      GMAUD8        G08DAT
033100071015     C                   MOVE      *ZEROS        G08INV
033200071015     C                   MOVEL     *BLANK        G08ERR
033300071015     C                   CALL      'XSRDA8'
033400071015     C                   PARM                    WLBDA8
033500071015     C                   Z-ADD     G08INV        AMGUD8            8 0
033600071015     C*
033700071015      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
033800071019     C     d54RIS        IFEQ      'R'
033900071019     C                   MOVE      d54DAT        G08INV
034000071015     C                   MOVE      *ZEROS        G08DAT
034100071015     C                   MOVEL     3             G08ERR
034200071015     C                   CALL      'XSRDA8'
034300071015     C                   PARM                    WLBDA8
034400071015     C                   Z-ADD     G08DAT        GMADRI            8 0
034500071015     C                   END
034600071015     C*
034700071015      * genero file di work in qtemp
034800071015     c                   Movel(p)  cmd2(1)       comman
034900071015     c                   eval      lenght = %len(%trim(comman))
035000071015     c                   Call      'QCMDEXC'                            97
035100071015     c                   Parm                    comman
035200071015     c                   Parm                    lenght
035300071015     c                   open      wfgcs00f
035400071015      * vista logica
035500071015     c                   Movel(p)  cmd4(1)       comman
035600071015     c                   eval      lenght = %len(%trim(comman))
035700071015     c                   Call      'QCMDEXC'                            97
035800071015     c                   Parm                    comman
035900071015     c                   Parm                    lenght
036000071015     c                   open      wfgcs01l
036100071015     c   97              except    errore
036200071015      *
036300071015     C* DEFINIZIONE CHIAVI
036400071015     C     Karbx         KLIST
036500071015     C                   KFLD                    FLE
036600071015     C                   KFLD                    datak             8 0
036700071019     C                   z-add     d54dat        datak
036800071015     C*
036900071015     C     Karb          KLIST
037000071015     C                   KFLD                    arbAAS
037100071015     C                   KFLD                    arbLNP
037200071015     C                   KFLD                    arbNRS
037300071015     C                   KFLD                    arbNSP
037400071015     C*
037500071015     C     Kart          KLIST
037600071015     C                   KFLD                    gcsAAS
037700071015     C                   KFLD                    gcsLNP
037800071015     C                   KFLD                    gcsNRS
037900071015     C                   KFLD                    gcsNSP
038000071015     C*
038100071015     C     Kblp          KLIST
038200071015     C                   KFLD                    LBLAAN
038300071015     C                   KFLD                    LBLLPN
038400071015     C                   KFLD                    LBLNRN
038500071015     C                   KFLD                    LBLNSN
038600071015     C*
038700071015     C     Kblt          KLIST
038800071015     C                   KFLD                    gcsAASf
038900071015     C                   KFLD                    gcsLNPf
039000071015     C                   KFLD                    gcsNRSf
039100071015     C                   KFLD                    gcsNSPf
039200071015     C*
039300071015     C     KTAB          KLIST
039400071015     C                   KFLD                    CODUT
039500071015     C                   KFLD                    COD               2
039600071015     C                   KFLD                    KEY               8
039700071026     c                   movel     *all'_'       lin198          198
039800071015     C*
039900071015     C***--------------------------------------------------------***
040000071015     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
040100071015     C***--------------------------------------------------------***
040200071015     c                   clear                   trul06ds
040300071015     c                   eval      d06cod = '£6'
040400071015     c                   movel     §fle          d06key
040500071015     c                   movel     trul06ds      kpjbu
040600071015     c                   call      'TRUL06R'
040700071015     c                   parm                    kpjba
040800071015     c                   movel     kpjbu         trul06ds
040900071015     c                   movea     lin           fil£6
041000071015     C                   ENDSR
041100071015     C*****************************************************************
041200071015     Ofnarbx00  E            AGG5
041300071015     O                       arbdst
041400071017     OPRTF198   E            TESTA          1 02
041500071015     O                       RAGUT             +  0
041600071015     O                       SIMFEL              25
041700071017     O                                         + 35 '* ELENCO SPEDIZIONI DI '
041800071017     O                                         +  0 'RESO O DIROTTAMENTO *'
041900071015     O                       §PGM               127
042000071015     O                       GMAUD8             147 '  /  /    '
042100071015     O                                         + 10 'PAG:'
042200071015     O                       PAGE          Z   +  1
042300071015     O          E            TESTA          1
042400071015     O                       KNSIF             +  0
042500071015     O                       KNMUS             +  0
042600071015     O                       ORA                147 '  :  :  '
042700071026     O          E            TESTA          1
042800071015     O                                         +  0 'CON DISPOSIZIONI DI:'
042900071026     O                       DESCR             +  1
043000071026     O          E            TESTA       1
043100071026     O                                         +  0 '* Prima riga bolla O'
043200071026     O                                         +  0 'riginale '
043300071026     O                                         +  0 'Seconda riga nuova '
043400071026     O                                         +  0 'Bolla *'
043500071026     O          E            TESTA          0
043600071026     O                                         +  0 '* Prima riga bolla O'
043700071026     O                                         +  0 'riginale '
043800071026     O                                         +  0 'Seconda riga nuova '
043900071026     O                                         +  0 'Bolla *'
044000071026     O          E            TESTA          0
044100071026     O                                         +  0 '* Prima riga bolla O'
044200071026     O                                         +  0 'riginale '
044300071026     O                                         +  0 'Seconda riga nuova '
044400071026     O                                         +  0 'Bolla *'
044500071015     O          E            TESTAR      1
044600071015     O                                         +  0 '---------  RISTAMPA'
044700071015     O                                         +  1 '---------  ELENCO DEL'
044800071015     O                       GMADRI            +  1 '  /  /    '
044900071015     O          E            TESTAT      2
045000071015     O                                            3 'LNP'
045100071015     O                                            8 'LNA'
045200071015     O                                           14 'ANNO'
045300071015     O                                           18 'SER.'
045400071015     O                                           27 'SPEDIZ.'
045500071019     O                                           38 'Mittente'
045600071019     O                                           80 'Destinatario'
045700071026     O                                          123 'F.Sgn'
045800071017     O                                          143 'Segnacol.Dal-Al'
045900071017     O                                          155 'Colli'
046000071017     O                                          166 'Peso Kg'
046100071017     O                                          173 'Zona'
046200140717     O                                          173 'Zona'
046300140717     O                                          182 'Spuntato'
046400071015     O          E            RIGA        2
046500071015     O                       GCsLNP               3
046600071015     O                       GCsLNA               8
046700071015     O                       GCsAAS              14
046800071015     O                       GCsNRS              18
046900071015     O                       GCsNSP        Z     27
047000071019     O                       gcsRSM              65
047100071015     O                       gcsRSD             103
047200071026     O                       gcsfls             122
047300071017     O                       gcsNCD        Z    133
047400071017     O                                          136 '/'
047500071019     O                       gcsNCA        Z    143
047600071019     O                       gcsNCL        Z    155
047700071017     O                       gcspkf        2    166
047800071106     O                       gcsZNC             172
047900071015     O          E            segn1       1
048000071015     O                       sk1(1)        z   + 30
048100071015     O                       sk1(2)        z   +  1
048200071015     O                       sk1(3)        z   +  1
048300071015     O                       sk1(4)        z   +  1
048400071015     O                       sk1(5)        z   +  1
048500071015     O                       sk1(6)        z   +  1
048600071015     O                       sk1(7)        z   +  1
048700071015     O                       sk1(8)        z   +  1
048800071015     O                       sk1(9)        z   +  1
048900071015     O                       sk1(10)       z   +  1
049000071015     O                       sk1(11)       z   +  1
049100071015     O                       sk1(12)       z   +  1
049200071015     O                       sk1(13)       z   +  1
049300071015     O                       sk1(14)       z   +  1
049400071015     O                       sk1(15)       z   +  1
049500071015     O                       sk1(16)       z   +  1
049600071015     O                       sk1(17)       z   +  1
049700071015     O                       sk1(18)       z   +  1
049800071015     O                       sk1(19)       z   +  1
049900071015     O                       sk1(20)       z   +  1
050000071015     O          E            segn2       1
050100071015     O                       sk2(1)        z   + 30
050200071015     O                       sk2(2)        z   +  1
050300071015     O                       sk2(3)        z   +  1
050400071015     O                       sk2(4)        z   +  1
050500071015     O                       sk2(5)        z   +  1
050600071015     O                       sk2(6)        z   +  1
050700071015     O                       sk2(7)        z   +  1
050800071015     O                       sk2(8)        z   +  1
050900071015     O                       sk2(9)        z   +  1
051000071015     O                       sk2(10)       z   +  1
051100071015     O                       sk2(11)       z   +  1
051200071015     O                       sk2(12)       z   +  1
051300071015     O                       sk2(13)       z   +  1
051400071015     O                       sk2(14)       z   +  1
051500071015     O                       sk2(15)       z   +  1
051600071015     O                       sk2(16)       z   +  1
051700071015     O                       sk2(17)       z   +  1
051800071015     O                       sk2(18)       z   +  1
051900071015     O                       sk2(19)       z   +  1
052000071015     O                       sk2(20)       z   +  1
052100071015     O          E            segn3       1
052200071015     O                       sk3(1)        z   + 30
052300071015     O                       sk3(2)        z   +  1
052400071015     O                       sk3(3)        z   +  1
052500071015     O                       sk3(4)        z   +  1
052600071015     O                       sk3(5)        z   +  1
052700071015     O                       sk3(6)        z   +  1
052800071015     O                       sk3(7)        z   +  1
052900071015     O                       sk3(8)        z   +  1
053000071015     O                       sk3(9)        z   +  1
053100070629     O                       sk3(10)       z   +  1
053200071015     O                       sk3(11)       z   +  1
053300071015     O                       sk3(12)       z   +  1
053400071015     O                       sk3(13)       z   +  1
053500071015     O                       sk3(14)       z   +  1
053600071015     O                       sk3(15)       z   +  1
053700071015     O                       sk3(16)       z   +  1
053800071015     O                       sk3(17)       z   +  1
053900071015     O                       sk3(18)       z   +  1
054000071015     O                       sk3(19)       z   +  1
054100071015     O                       sk3(20)       z   +  1
054200070629     O          E            segn4       1
054300071015     O                       sk4(1)        z   + 30
054400071015     O                       sk4(2)        z   +  1
054500071015     O                       sk4(3)        z   +  1
054600071015     O                       sk4(4)        z   +  1
054700071015     O                       sk4(5)        z   +  1
054800070629     O                       sk4(6)        z   +  1
054900071015     O                       sk4(7)        z   +  1
055000071015     O                       sk4(8)        z   +  1
055100071015     O                       sk4(9)        z   +  1
055200071015     O                       sk4(10)       z   +  1
055300071015     O                       sk4(11)       z   +  1
055400071015     O                       sk4(12)       z   +  1
055500071015     O                       sk4(13)       z   +  1
055600071015     O                       sk4(14)       z   +  1
055700071015     O                       sk4(15)       z   +  1
055800071015     O                       sk4(16)       z   +  1
055900071015     O                       sk4(17)       z   +  1
056000071015     O                       sk4(18)       z   +  1
056100071015     O                       sk4(19)       z   +  1
056200071015     O                       sk4(20)       z   +  1
056300071015     O          E            segn5       1
056400071015     O                       sk5(1)        z   + 30
056500071015     O                       sk5(2)        z   +  1
056600071015     O                       sk5(3)        z   +  1
056700071015     O                       sk5(4)        z   +  1
056800071015     O                       sk5(5)        z   +  1
056900071015     O                       sk5(6)        z   +  1
057000071015     O                       sk5(7)        z   +  1
057100071015     O                       sk5(8)        z   +  1
057200071015     O                       sk5(9)        z   +  1
057300071015     O                       sk5(10)       z   +  1
057400071015     O                       sk5(11)       z   +  1
057500071015     O                       sk5(12)       z   +  1
057600071015     O                       sk5(13)       z   +  1
057700071015     O                       sk5(14)       z   +  1
057800071015     O                       sk5(15)       z   +  1
057900071015     O                       sk5(16)       z   +  1
058000071015     O                       sk5(17)       z   +  1
058100071015     O                       sk5(18)       z   +  1
058200071015     O                       sk5(19)       z   +  1
058300071015     O                       sk5(20)       z   +  1
058400071015     O          E            RIGAf       1
058500071015     O                       GCsLNPf              3
058600071015     O                       GCsLNAf              8
058700071015     O                       GCsAASf             14
058800071015     O                       GCsNRSf             18
058900071015     O                       GCsNSPf       Z     27
059000071015     O                       gcsRSMf             65
059100071015     O                       gcsRSDf            103
059200071026     O                       gcsflsf            122
059300071017     O                       gcsNCDf       Z    133
059400071017     O                                          136 '/'
059500071019     O                       gcsNCAf       Z    143
059600071019     O                       gcsNCLf       Z    155
059700071017     O                       gcspkff       2    166
059800071106     O                       gcsZNCf            172
059900140728     O                       spuntato           182
060000071017     O          E            segn1f      1
060100071015     O                       sk1f(1)       z   + 30
060200071015     O                       sk1f(2)       z   +  1
060300071015     O                       sk1f(3)       z   +  1
060400071015     O                       sk1f(4)       z   +  1
060500071015     O                       sk1f(5)       z   +  1
060600071015     O                       sk1f(6)       z   +  1
060700071015     O                       sk1f(7)       z   +  1
060800071015     O                       sk1f(8)       z   +  1
060900071015     O                       sk1f(9)       z   +  1
061000071015     O                       sk1f(10)      z   +  1
061100071015     O                       sk1f(11)      z   +  1
061200071015     O                       sk1f(12)      z   +  1
061300071015     O                       sk1f(13)      z   +  1
061400071015     O                       sk1f(14)      z   +  1
061500071015     O                       sk1f(15)      z   +  1
061600071015     O                       sk1f(16)      z   +  1
061700071015     O                       sk1f(17)      z   +  1
061800071015     O                       sk1f(18)      z   +  1
061900071015     O                       sk1f(19)      z   +  1
062000071015     O                       sk1f(20)      z   +  1
062100071017     O          E            segn2f      1
062200071015     O                       sk2f(1)       z   + 30
062300071015     O                       sk2f(2)       z   +  1
062400071015     O                       sk2f(3)       z   +  1
062500071015     O                       sk2f(4)       z   +  1
062600071015     O                       sk2f(5)       z   +  1
062700071015     O                       sk2f(6)       z   +  1
062800071015     O                       sk2f(7)       z   +  1
062900071015     O                       sk2f(8)       z   +  1
063000071015     O                       sk2f(9)       z   +  1
063100071015     O                       sk2f(10)      z   +  1
063200071015     O                       sk2f(11)      z   +  1
063300071015     O                       sk2f(12)      z   +  1
063400071015     O                       sk2f(13)      z   +  1
063500071015     O                       sk2f(14)      z   +  1
063600071015     O                       sk2f(15)      z   +  1
063700071015     O                       sk2f(16)      z   +  1
063800071015     O                       sk2f(17)      z   +  1
063900071015     O                       sk2f(18)      z   +  1
064000071015     O                       sk2f(19)      z   +  1
064100071015     O                       sk2f(20)      z   +  1
064200071017     O          E            segn3f      1
064300071015     O                       sk3f(1)       z   + 30
064400071015     O                       sk3f(2)       z   +  1
064500071015     O                       sk3f(3)       z   +  1
064600071015     O                       sk3f(4)       z   +  1
064700071015     O                       sk3f(5)       z   +  1
064800071015     O                       sk3f(6)       z   +  1
064900071015     O                       sk3f(7)       z   +  1
065000071015     O                       sk3f(8)       z   +  1
065100071015     O                       sk3f(9)       z   +  1
065200071015     O                       sk3f(10)      z   +  1
065300071015     O                       sk3f(11)      z   +  1
065400071015     O                       sk3f(12)      z   +  1
065500071015     O                       sk3f(13)      z   +  1
065600071015     O                       sk3f(14)      z   +  1
065700071015     O                       sk3f(15)      z   +  1
065800071015     O                       sk3f(16)      z   +  1
065900071015     O                       sk3f(17)      z   +  1
066000071015     O                       sk3f(18)      z   +  1
066100071015     O                       sk3f(19)      z   +  1
066200071015     O                       sk3f(20)      z   +  1
066300071017     O          E            segn4f      1
066400071015     O                       sk4f(1)       z   + 30
066500071015     O                       sk4f(2)       z   +  1
066600071015     O                       sk4f(3)       z   +  1
066700071015     O                       sk4f(4)       z   +  1
066800071015     O                       sk4f(5)       z   +  1
066900071015     O                       sk4f(6)       z   +  1
067000071015     O                       sk4f(7)       z   +  1
067100071015     O                       sk4f(8)       z   +  1
067200071015     O                       sk4f(9)       z   +  1
067300071015     O                       sk4f(10)      z   +  1
067400071015     O                       sk4f(11)      z   +  1
067500071015     O                       sk4f(12)      z   +  1
067600071015     O                       sk4f(13)      z   +  1
067700071015     O                       sk4f(14)      z   +  1
067800071015     O                       sk4f(15)      z   +  1
067900071015     O                       sk4f(16)      z   +  1
068000071015     O                       sk4f(17)      z   +  1
068100071015     O                       sk4f(18)      z   +  1
068200071015     O                       sk4f(19)      z   +  1
068300071015     O                       sk4f(20)      z   +  1
068400071017     O          E            segn5f      1
068500071015     O                       sk5f(1)       z   + 30
068600071015     O                       sk5f(2)       z   +  1
068700071015     O                       sk5f(3)       z   +  1
068800071015     O                       sk5f(4)       z   +  1
068900071015     O                       sk5f(5)       z   +  1
069000071015     O                       sk5f(6)       z   +  1
069100071015     O                       sk5f(7)       z   +  1
069200071015     O                       sk5f(8)       z   +  1
069300071015     O                       sk5f(9)       z   +  1
069400071015     O                       sk5f(10)      z   +  1
069500071015     O                       sk5f(11)      z   +  1
069600071015     O                       sk5f(12)      z   +  1
069700071015     O                       sk5f(13)      z   +  1
069800071015     O                       sk5f(14)      z   +  1
069900071015     O                       sk5f(15)      z   +  1
070000071015     O                       sk5f(16)      z   +  1
070100071015     O                       sk5f(17)      z   +  1
070200071015     O                       sk5f(18)      z   +  1
070300071015     O                       sk5f(19)      z   +  1
070400071015     O                       sk5f(20)      z   +  1
070500071026     O          E            separa      1
070600071026     o                       lin198             198
070700071015     O          E            errRIGAf    1
070800071015     O                                         + 25 '* Attenzione non trovata '
070900071015     O                                         +  0 'spedizione figlia verifi'
071000071015     O                                         +  0 'care i dati della bolla *'
071100071015     O          E            errore         1 02
071200071015     O                       RAGUT             +  0
071300071015     O                       SIMFEL              25
071400071015     O                                         + 25 '* ELENCO SPEDIZIONI DA '
071500071015     O                                         +  0 'PORTARE NEL MAGAZZINO '
071600071015     O                                         +  0 'GIACENZE *'
071700071015     O                       §PGM               127
071800071015     O                       GMAUD8             147 '  /  /    '
071900071015     O                                         + 10 'PAG:'
072000071015     O                       PAGE          Z   +  1
072100071015     O          E            errore      2
072200071015     O                                         + 25 '* Attenzione errore in '
072300071015     O                                         +  0 'esecuzione programma  '
072400071015     O                                         +  0 'avvisare il CED *'
072500071015      *_________________________________________________________________________
072600071015**  CMD1
072700071015DLTF FILE(QTEMP/WFGCS00F)
072800071015**  CMD2
072900071015CRTDUPOBJ OBJ(WFGCS00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
073000071015**  CMD3
073100071015DLTF FILE(QTEMP/WFGCS01L)
073200071015**  CMD4
073300071011CRTDUPOBJ OBJ(WFGCS01L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
