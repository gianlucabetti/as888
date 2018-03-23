000100151111     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PRNPGM') ACTGRP('QILE')
000200151111     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400151111     Ftrmz851f  if   e           k disk
000500151111     Ftrmz851f0 uf a e           k disk    usropn
000600150527     D*-------------
000700020304     dkpjba          e ds
000800151111     D trmz70s1ds    E DS                  prefix(S1_)
000900151111     D*-------------
001000070605     D WLBDA8          DS
001100070605     D  G02DAT                 1      8  0
001200070605     D  G02INV                 9     16  0
001300070605     D  G02ERR                17     17
001400070605     D  G02TGI                18     22  0
001500151111     D*-------------
001600090519     d coman           s             99
001700090519     d luncmd          s             15  5
001800090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001900090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002000090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002100090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002200090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002300000000     C*---------------------------------------------------------------*
002400151111      *  rilegge il file appena generato e ne crea uno analogo con i dati
002500151111      *  anagrafici di PROJ
002600151111      *
002700151111     c     *loval        setll     trmz851f
002800151111      *
002900151111     c                   do        *hival
003000151111     c                   read      trmz851f                               98
003100151111     c   98              leave
003200151111     c                   clear                   trmz85100
003300151111      *
003400151111     c                   exsr      elabora
003500151111     c                   enddo
003600150527      *
003700090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
003800090519     c                   movea     cmd4          coman
003900090519     c                   exsr      comandi
004000090519      *esegue crtdupobj da qtemp a wrkquery
004100090519     c                   movea     cmd5          coman
004200090519     c                   exsr      comandi
004300150527      *
004400090519      *elimina file in qtemp x la filiale
004500151111     c                   close     trmz851f0
004600090519     c                   movea     cmd3          coman
004700150527      *
004800090519     c                   exsr      comandi
004900150527      *
005000020308     c                   seton                                        lr
005100050802     c*-------------------------------------------------
005200090518     c     elabora       begsr
005300050802     c*-------------------------------------------------
005400151111     c                   exsr      Decod_Rag_IVA
005500151111      *
005600151111     c                   eval      w851DIST   =  T851DIST
005700151111     c                   eval      w851AREA   =  T851AREA
005800151111     c                   eval      w851AREAD  =  T851AREAD
005900151111     c                   eval      w851FIL    =  T851FIL
006000151111     c                   eval      w851FILD   =  T851FILD
006100151111     c                   eval      w851SOCG   =  T851SOCG
006200151111     c                   eval      w851SOCGD  =  T851SOCGD
006300151111     c                   eval      w851IVA    =  T851IVA
006400151111     c                   eval      w851RSOC   =  T851RSOC
006500151111     c                   eval      w851DDURC  =  T851DDURC
006600151111     c                   eval      w851DCIP   =  T851DCIP
006601160222     c                   eval      w851cor    =  T851cor
006700151111     c                   eval      w851DIA    =  T851DIA
006800151111     c                   eval      w851DEC    =  T851DEC
006900151111     c                   eval      w851DFC    =  T851DFC
007000151111     c                   eval      w851NRC    =  T851NRC
007100151111     c                   eval      w851IPIVA  =  T851IPIVA
007200151111     c                   eval      w851NOTE   =  T851NOTE
007300151111     c                   eval      w851NATG   =  T851NATG
007400151111     c                   eval      w851NATGD  =  T851NATGD
007500151111     c                   eval      w851IVACOP =  T851IVACOP
007600151111     c                   eval      w851RSOCOP =  T851RSOCOP
007700151111     c                   eval      w851TIPO   =  T851TIPO
007800151111     c                   eval      W851RGSOC1 =  S1_RAGSOCKSC
007900151111     c                   eval      W851RGSOC2 =  S1_RAGSOCKSC1
008000151111     c                   eval      W851INDIR  =  S1_INDIRIZZO
008100151111     c                   eval      W851LOCAL  =  S1_LOCALITA
008200151111     c                   eval      W851CAP    =  S1_CAP
008300151111     c                   eval      W851PROV   =  S1_PROVINCIA
008400151111     c                   eval      W851STATO  =  S1_STATO
008500070530      *
008600151111     c                   write     TRMZ85100
008700151111      *
008800070530     c                   endsr
008900151111     c*--------------------------------------------------------------
009000151111     c* tramite Società e Unità decodifica P.IVA su PROJ
009100151111     c*--------------------------------------------------------------
009200151111     C     Decod_Rag_IVA BegSR
009300151111      **
009400151111      **
009500151111      **  Routine x Reperire dati Fornitore:
009600151111     C                   clear                   trmz70s1ds                     Input
009700151111     C                   movel(p)  T851IVA       s1_PartitaIVA                  Input
009800151111     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
009900151111     C                   movel(p)  T851SOCG      s1_Societa                     Input/Output
010000151111      * solo al primo tentativo carica la filiale
010100151111      *   in un secondo la lascia blank
010200151111     C                   movel(p)  T851FIL       s1_Unita                       Input/Output
010300151111     c*
010400151111     c                   call      'TRMZ70SR1'
010500151111     C                   PARM                    trmz70s1ds                     Input
010600151111      *
010700151111     c                   if         S1_Trovato = *On and S1_Errore = '0'
010800151111      *
010900151111     c                   else
011000151111      *   in un secondo la lascia blank
011100151111     C                   clear                   s1_Unita                       Input/Output
011200151111     c                   call      'TRMZ70SR1'
011300151111     C                   PARM                    trmz70s1ds                     Input
011400151111     c                   if         S1_Trovato = *On and S1_Errore = '0'
011500151111      *
011600151111     c                   else
011700151111      *   se errore svuota la DS
011800151111     C                   clear                   trmz70s1ds                     Input
011900151111     c                   end
012000151111     c                   end
012100151111      *
012200151111     C                   ENDSR
012300151111     C/EJECT
012400150612     c*-------------------------------------------------
012500150612     c     comandi       begsr
012600150612     c*-------------------------------------------------
012700090519     C                   z-add     99            luncmd
012800090519     C                   CALL      'QCMDEXC'                            34
012900090519     C                   parm                    coman
013000090519     C                   parm                    luncmd
013100090519     c                   endsr
013200150309     c*-------------------------------------------------
013300150309     c     *inzsr        begsr
013400150309     c*-------------------------------------------------
013500020304     c     *entry        plist
013600020304     c                   parm                    kpjba
013700090519      *clear file
013800090519     c                   movea     cmd1          coman
013900090519     c                   exsr      comandi
014000090519     c                   movea     cmd2          coman
014100090519     c                   exsr      comandi
014200151111     c                   open      trmz851f0
014300090519
014400090518     c                   endsr
014500090519**  CMD1
014600151111CLRPFM    FILE(trmz851f0)
014700090519**  CMD2
014800151111CRTDUPOBJ OBJ(trmz851F0) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
014900090519**  CMD3
015000151111DLTF FILE(QTEMP/trmz851F0)
015100090519**  CMD4
015200151111dltf file(WRKQUERY/trmz851f0)
015300090519**  CMD5
015400151111CRTDUPOBJ OBJ(trmz851F0) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz851f0) DATA(*YES)
