000100130405     /*END
000200030908      *--------------------------------------------------------------------------------------------*
000300130409      * Calcolo saldi (FNMSS10F) per letture NON VDL                                               *
000400030908      *--------------------------------------------------------------------------------------------*
000500030908
000600000000     h DECEDIT('0,') DATEDIT(*DMY.)
000700030908
000800130405     fTNTBE01L  if   e           k disk
000900130405     fAZORG01L  if   e           k disk
001000030908
001100130409     d jj              S              3  0
001200000000     d kbecod          S                   LIKE(tbecod)                         *tntbe00f
001300000000     d kbeke1          S                   LIKE(tbeke1)
001400030908     d kbeke2          S                   LIKE(tbeke2)
001500030908     d comman          S             80    INZ('DLYJOB DLY(15)')                *comando CLP
001600030908     d lenght          S             15  5 INZ(45)                              *lunghezza comando
001700030908      *---
001800130405     d mftFil1         S              3    DIM(29)  INZ                         *Fil. SI tabellone
001900130405     d mftApl1         S              1    DIM(29)  INZ                         *Tipo applicaz.
002000130405      *---
002100130405     d sk_Fil1         s              3    dim(999) inz                         *sch. Fil. NO tabell
002200130405     d sk_Apl1         s              1    dim(999) inz                         *sch. Tipo Applicaz.
002300020820      *---
002400130405     d dmft          E DS                  inz
002500130405     d Og148         e ds                  inz   qualified
002600130405     d Og150         e ds                  inz   qualified
002700130405      *---
002800000000     d param           DS                  INZ                                  *DS parametri
002900130405     d  parfil                 1      3  0 inz                                   - filiale
003000130405     d  parapl                 4      4                                          - tipo applicazione
003100030908
003200030908      *--------------------------------------------------------------------------------------------*
003300030908     c     keytbe        KLIST                                                  *tntbe02l
003400030908     c                   KFLD                    kbecod                          -tabella
003500030908     c                   KFLD                    kbeke1                          -chiave uno
003600030908     c                   KFLD                    kbeke2                          -chiave uno
003700030908      *
003800030908      * Imposta il fine ciclo
003900030908     c                   eval      *in98 = *off
004000030908      *
004100030908      * Fino a che non chiude il sottosistema
004200030908    1c                   DOW       NOT *in98
004300030908      *
004400030908      * Filiali con tabellone
004500030908     c                   CLEAR                   mftFil1                        *campone filiali
004600130405     c                   CLEAR                   mftApl1                        *campone tipo applic
004700030908     c                   MOVEL     'MFT'         kbecod                         *tabella
004800030908     c                   MOVEL(P)  946           kbeke1                         *chiave 1 = elaborat
004900130405     c                   clear                   kbeke2
005000030908     c     keytbe        CHAIN     tntbe01l
005100030908    2c                   IF        %found(TNTBE01L) and TBEatb = *blanks
005200030908     c                   MOVEL     tbeuni        dmft
005300030908     c                   MOVEA     §mftfil       mftFil1
005400030908     c                   MOVEA     §mftapl       mftApl1
005500030908    2c                   ENDIF
005600130405      *
005700130405      * Filiali SENZA tabellone
005800130405     c     *loval        setll     AZORG
005900130405     c                   read      AZORG
006000130405do  2c                   DOW       Not  %eof(AZORG01L)
006100130405if  3c                   If        ORGfva = *blank  and
006200130405     c                             %lookup( %editc( ORGfil : 'X' ) :
006300130405     c                                      mftFil1 ) = *zeros
006400130405     c                   eval      Og148  = ORGde8
006500130405     c                   eval      Og150  = ORGdf0
006600130405if  4c                   If        Og148.§ogLPO <> *blanks  and
006700130405     c                             Og150.§ogTIS <> *blanks
006800130409     c                   eval      jj += 1
006900130409     c                   eval      sk_Fil1(jj) = %editc( ORGfil : 'X' )
007000130409     c                   eval      sk_Apl1(jj) = Og150.§ogTIS
007100130405e   4c                   EndIf
007200130405e   3c                   EndIf
007300130405     c                   read      AZORG
007400130405e   2c                   ENDDO
007500030908      *
007600130405      * Ciclo fino a filiali SENZA tabellone (sull'AS/400)
007700130409    2c                   FOR       jj = 1  By 1  To %elem(sk_Fil1)
007800130409    3c                   IF        sk_Fil1(jj) <= *zeros
007900130405     c                   leave
008000130405     c                   ENDIF
008100030908      * Chiama il programma di aggiornamento archivi
008200130410      * » passo comunque il tipo applicazione VDL,
008300130410      *   perchè altrimenti verrebbero memorizzati anche questi dati
008400130410      *   nel file FLTR800F «
008500130409     c                   move      sk_Fil1(jj)   parfil                         *filiale
008600130410     c*//                movel     sk_Apl1(jj)   parapl                         *tipo applicazione
008700130410     c                   movel     mftApl1(01)   parapl                         *tipo applicaz. VDL
008800001027     c                   CALL      'FIMS22R'
008900000000     c                   PARM                    param
009000130405    2c                   ENDFOR                                                 *fine filiali
009100030908      *
009200030908      * Attende qualche secondo
009300030908     C                   CALL      'QCMDEXC'
009400030908     C                   PARM                    comman
009500030908     C                   PARM                    lenght
009600030908      *
009700030908      * Testa la chiusura del sottosistema
009800000000     c                   SHTDN                                        98
009900030908    1c                   ENDDO                                                  *fine sottosistema
010000030908      *
010100030908     c                   EVAL      *INLR = *ON
010200030908      *--------------------------------------------------------------------------------------------*
