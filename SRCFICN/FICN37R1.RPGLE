000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000201120827     h dftactgrp(*no) actgrp(*caller)
000202120827ab   H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000300000000     H*--------------------------------------------------------------*
000400030110     Ffictt02l  uf   e           k disk
000500030110     Ffictd01l  uf   e           k disk
000600030120     Ffiapd01l  if   e           k disk
000700030205     fqsysprt   o    f  132        printer infds(stamp)
000800011026
000900030110     D KPJBA         E DS
001000030110     D dsparam         DS
001100030110     D  parpo                         3s 0
001200030110     D  data                          8s 0
001300030219     D  error                         1
001400030205     d stamp           ds
001500030205     d  lin                  367    368b 0
001600030110     D WLBDAT          DS
001700030110     D  G02DAT                 1      8  0
001800030110     D  G02INV                 9     16  0
001900030110     D  G02ERR                17     17
002000030110     D  G02TGI                18     22  0
002100030219     d dataiso         s               d   datfmt(*iso)
002200030219     d dataeur         s               d   datfmt(*eur)
002201120827ab   ***************************************************************************
002202120827ab   ** Prototipi.
002203120827ab   ***************************************************************************
002204120827ab    **
002205120827ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
002206120827ab    /DEFINE DFTACTGRP_NO
002207120827ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
002208120827ab    /UNDEFINE DFTACTGRP_NO
002209120827ab   **
002210120827ab   ***************************************************************************
002211120827ab   **       Campi
002212120827ab   ***************************************************************************
002213120827ab   D TibsSof_esito...
002214120827ab   D                 S             10I 0 IMPORT
002215120827ab   D idSocieta...
002216120827EDPDCD                 S              3A
002217120827ab   **
002218120827EDPDCd Filiale         s              3S 0
002219120827EDPDCd dtVALIDITA      s               D
002222120827EDPDCd VALdatFINE      s               D
002223120827EDPDCd VALdatINIZ      s               D
002224120827EDPDCd SOC_errata      s              1A
002300000000     C*---------------------------------------------------------------*
002400030110     c                   exsr      fictt
002500030219     c                   movel     dsparam       kpjbu
002600030710     c                   if        error = *blank
002700030710     c                   except    tuttok
002800030710     c                   endif
002900990113     c                   seton                                        lr
003000990113     c*-------------------------------------------------
003100030110     c     fictt         begsr
003200990113     c*-------------------------------------------------
003300030110     c     parpo         setll     fictt02l
003400030110     c                   do        *hival
003500030110     c     parpo         reade     fictt02l                               99
003600030110     c   99              leave
003601120827     c*
003602120827     c     kapd          chain     fiapd01l                           96
003603120828     c                   if        *in96
003604120827     c                   iter
003605120827     c                   end
003700030110      * se la data passa il limite immesso esco dal loop
003800030110     c                   if        cttddc > data
003900030110     c                   leave
004000030110     c                   endif
004100030214      * conferma solo il tipo servizio 'X' relativo alle coop
004200030214      * eventuali testate relative a PULIZIE O TRATTORISTI vengono confermate
004300030214      * specificatamente in altro contesto
004400061006     c                   if        ctttsr <> 'X' or cttatb = 'A'
004500030214     c                   iter
004600030214     c                   endif
004700120827      * controllo che la società operativa della filiale sia corretta
004701120827     C     *iso          move      cttDDC        dtVALIDITA
004702120827     C                   MOVEL     cttPDR        Filiale
004703120827     c                   exsr      chk_SOC_ope
004704120828     c* fino a quando non si eliminerà la memorizzazione del codice in
004705120828     c* valorizzazione devo testare il codice dei conteggi, poi testerò
004706120828     c* solo quello dell'anagrafica
004708120827     C                   if        soc_errata ='S'  or
004712120829     C                             apdcsf <> idSocieta
004715120827     c                   movel     '2'           error
004716120827     c                   leavesr
004717120827     C                   end
004801120827      * i manca tariffa non possono essere confermati vengono segnalati
004802120827      * tramite spool
004900030205     c                   if        cttfla = 'S'
005000030219     c                   move      cttddc        dataiso
005100030219     c                   move      dataiso       dataeur
005200030219     c                   move      dataeur       cttddcs           8 0
005300030205     c                   except    notariffa
005400030219     c                   movel     '1'           error
005500030205     c                   iter
005600030205     c                   endif
005700020211
005800030110     c                   exsr      dettaglio
005900030110
006000030120     c                   z-add     cttied        cttiec
006100030110     c                   z-add     g02inv        cttdcv
006102120827     c                   move      *all'0'       cttcdf
006103120827     c                   move      apdksc        cttcdf
006104120827     c                   move      apdcsf        cttsoc
006200030110     c                   move      'C'           cttfvl
006300030214      * se non sono state impostate le ore lavorate arrivo/partenza
006400030214      *imposto il valore presente in anagrafica prima di consolidare
006500030214     c                   if        ctthpa = 0 and ctthar = 0
006800030214     c                   z-add     apdhpa        ctthpa
006900030214     c                   z-add     apdhar        ctthar
007000030214     c                   endif
007200030214
007300030110     c                   update    fictt000
007400030110     c                   enddo
007500020110
007600990113     c                   endsr
007700020211     c*-------------------------------------------------
007800020211     c     dettaglio     begsr
007900020211     c*-------------------------------------------------
008000030110     c     kctd          setll     fictd01l
008100020211     c                   do        *hival
008200030110     c     kctd          reade     fictd01l                               97
008300020211     c   97              leave
008400020211
008500030110     c                   z-add     ctdied        ctdiec
008600030110     c                   update    fictd000
008700020211     c                   enddo
008800020211
008900020211     c                   endsr
008901120827ab   C**************************************************************************
008902120827ab    * Controlla la Società operativa della Filiale
008903120827ab   C**************************************************************************
008904120827ab   C     CHK_SOC_OPE   begsr
008905120827ab   **
008906120827     c                   eval      soc_errata ='N'
008907120827ab    /FREE
008908120827ab
008909120827EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
008910120827EDPDC                                             : dtVALIDITA
008911120827EDPDC                                             : 'O'
008912120827EDPDC                                             : valDatIniz
008913120827EDPDC                                             : valDatFine
008914120827ab                                                );
008915120827ab
008916120827EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
008917120827EDPDC    // Gestire l'errore.
008918120827
008919120827         eval  soc_errata ='S';
008920120827
008921120827ab     ENDIF;
008922120827ab
008923120827ab    /END-FREE
008924120827ab   C                   ENDSR
009000011026     c*-------------------------------------------------
009100011026     c     *inzsr        begsr
009200011026     c*-------------------------------------------------
009300020211     c     *entry        plist
009400030110     c                   parm                    kpjba
009500030110     c                   movel     kpjbu         dsparam
009600030205
009700030110     c     kctd          klist
009800030110     c                   kfld                    cttfgs
009900030110     c                   kfld                    cttddc
010000030110
010100030120     c     kapd          klist
010200030120     c                   kfld                    tipoa             1
010300030120     c                   kfld                    cttpdr
010400030120     c                   move      'C'           tipoa
010500030120
010600030110     C                   TIME                    W0120            14 0
010700030110     C                   MOVE      W0120         WDAT              8 0
010800030110     C*
010900030110     C                   Z-ADD     WDAT          G02DAT
011000030110     C                   MOVEL     *BLANK        G02ERR
011100030110     C                   CALL      'XSRDA8'
011200030110     C                   PARM                    WLBDAT
011300030205
011400030205     c                   z-add     66            lin
011500030205     c                   except    testa
011600011026
011601120829ab   C*     Inizializza
011602120829ab    /FREE
011603120829ab     TibsSof_Init();
011604120829ab    /END-FREE
011605120829     C*
011700011026     c                   endsr
011800011129     C*--------------------------------------------------------------------
011900030205     O*------------------------------------------------------*
012000030205     OQSYSPRT   e            TESTA             2
012100030205     O                                           70 'Anomalie in conferma -
012200030205     O                                              valorizzazione Coop'
012300030205     O          e            notariffa   1
012400030205     o                                              'Presenti manca tariffa -
012500030219     o                                              verificare le testate -
012600030219     o                                              o la tariffa x la data'
012700030219     o                       cttddcs            + 2 '  /  /    '
012800030205     o                                          + 2 'Filiale'
012900030205     o                       cttfgs        z    + 2
013000030205     o                                          + 2 'Cod.Coop'
013100030205     o                       cttpdr        z    + 2
013200030710     O          e            tuttok      1
013300030710     o                                              'C O N F E R M A   O K  -
013400030710     o                                              SI PUO PROCEDERE CON  -
013500030710     o                                              LA CONTABILIZZAZIONE '
