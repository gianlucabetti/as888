000100150513     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200150513     H BNDDIR('QC2LE')
000300060727     H DECEDIT('0,') DATEDIT(*DMY.)
000400140319     FWfDPDbai  UF   E             DISK    rename(test:WFDPDW)
000500140319     FWfDPDba   O    E             DISK
000600150513     FWfDPDba0F O    E             DISK
000700150513      * ?   * Campi da tradurre con interi
000800150513     D atoi            PR            10I 0
000900150513     D                                     EXTPROC('atoi')
001000150513     D  Char                           *
001100150513     D                                     VALUE
001200150513     D                                     OPTIONS(*STRING)
001300150513      * ?   * Campi da tradurre con decimali
001400150513     D atof            PR             8F
001500150513     D                                     EXTPROC('atof')
001600150513     D  Char                           *
001700150513     D                                     VALUE
001800150513     D                                     OPTIONS(*STRING)
001900150513      * ?   *--------------------------------------------------------------*
002000150513     D TIDPCICS      E DS                  PREFIX(DBA:3)
002100060727      * ?================================================================== */
002200060726      * ?   *--------------------------------------------------------------*
002300060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
002400060726      * ?   *--------------------------------------------------------------*
002500060726      *
002600060726      * ?   * Campi x decodifica * (INPUT  del Record)
002700060726     D  Dati           s           1500
002800060726     D Separa_campi    s              1
002900060726     D Decimal_Separ   s              1
003000060726      * ?   * Campi decodificati * (OUTPUT del Record)
003100060726      *       Schiere di Output / Campi di Output
003200060726     D  Alfa40         s             40
003300060726     d  Sk             s              3u 0
003400060726     D    NR_campi     s              3u 0
003500060726     D  Campi_Record   ds
003600060726     D    Dato_sk                    40    DIM(100)
003700060726     D  Campi_Lunghi   ds
003800060726     D    LunDato_sk                  3u 0 DIM(100)
003900060726     D Campi_Numerici  ds
004000060726     D    DatoNum_sk                  1    DIM(100)
004100060726     D Campi_Decimali  ds
004200060726     D    Decimal_sk                  3u 0 DIM(100)
004300150513      *
004400150513     D numeri_ammessi  C                   '0123456789,'
004500060726      * ?================================================================== */
004600060726      *   Ciclo principale
004700060726      * ?================================================================== */
004800140312
004900060726      *   Il primo è la testata già controllata dalla Check_trasm.
005000140319     c                   read      WFDPDbai
005100140319     c                   dow       not %eof(WFDPDbai)
005200081202      ** Decodifica record a campi variabili
005300140227     c                   exsr      Decod_Record
005400140409     c                   delete    WFDPDW
005500140409     c                   read      WFDPDbai
005600060726     C                   ENDdo
005700060726      **
005800060726     c                   SETON                                        LR
005900060726      * ?______________________________________________________________     */
006000060726      *?    Decodifica record a campi variabili
006100060726      * ?______________________________________________________________     */
006200140227     c     Decod_Record  Begsr
006300140227
006400140227      * ? Sposta il record a tracciato libero su un campo di lavoro       =
006500140303     c                   clear                   deb_cred          1
006600140319     c                   movel(p)  f1            dati
006700140227     c                   eval      dati = %Trim(Dati) + ';'
006800140227     c                   exsr      Split_Rec
006900140227     c                   exsr      Rec_Dettaglio
007000140227      **
007100140227     c                   Endsr
007200081202      * ?_______________________________________________________________    */
007300081202      *?      Riga da controllare e tradurre
007400081202      * ?_______________________________________________________________    */
007500140227     c     Rec_Dettaglio Begsr
007600150513      *
007700150513      *
007800140319     c                   clear                   TIDPC000
007900140319     c                   eval      DPCPRN     = Dato_sk(1)
008000140319     c                   eval      DPCYMO     = Dato_sk(2)
008100140319     c                   eval      DPCPROD    = Dato_sk(3)
008200140319     c                   eval      DPCCRBU    = Dato_sk(4)
008300140319     c                   eval      DPCDEBU    = Dato_sk(5)
008400140319     c                   eval      DPCCRBUC   = Dato_sk(6)
008500140319     c                   eval      DPCDEBUC   = Dato_sk(7)
008600140319     c                   eval      DPCCRSAP   = Dato_sk(8)
008700140319     c                   eval      DPCDESAP   = Dato_sk(9)
008800140319     c                   eval      DPCCRSTE   = Dato_sk(10)
008900140319     c                   eval      DPCDESTE   = Dato_sk(11)
009000140319     c                   eval      DPCPROC    = Dato_sk(12)
009100140319     c                   eval      DPCELEM    = Dato_sk(13)
009200140319     c                   eval      DPCRAKE    = Dato_sk(14)
009300140319     c                   eval      DPCWGHT    = Dato_sk(15)
009400140319     c                   eval      DPCQTY     = Dato_sk(16)
009500140319     c                   eval      DPCVAL     = Dato_sk(17)
009600140319     c                   eval      DPCVAT     = Dato_sk(18)
009700140319     c                   eval      DPCCREDP   = Dato_sk(19)
009800140319     c                   eval      DPCDEBDP   = Dato_sk(20)
009900140319     c                   eval      DPCAMO     = Dato_sk(21)
010000140319     c                   write     TIDPC000
010100150513      **
010200150513     c                   clear                   TIDPCICS
010300150513     c                   eval      DBAPRN     = Dato_sk(1)
010400150513     c                   eval      DBAYMO     = Dato_sk(2)
010500150513     c                   eval      DBAPROD    = Dato_sk(3)
010600150513     c                   eval      DBACRBU    = Dato_sk(4)
010700150513     c                   eval      DBADEBU    = Dato_sk(5)
010800150513     c                   eval      DBACRBUC   = Dato_sk(6)
010900150513     c                   eval      DBADEBUC   = Dato_sk(7)
011000150513     c                   eval      DBACRSAP   = Dato_sk(8)
011100150513     c                   eval      DBADESAP   = Dato_sk(9)
011200150513     c                   eval      DBACRSTE   = Dato_sk(10)
011300150513     c                   eval      DBADESTE   = Dato_sk(11)
011400150513     c                   eval      DBAPROC    = Dato_sk(12)
011500150513     c                   eval      DBAELEM    = Dato_sk(13)
011600150513     c                   eval      DBARAKE    = Dato_sk(14)
011700150513     c                   eval      DBAWGHT    = Dato_sk(15)
011800150513     c                   eval      DBACREDP   = Dato_sk(19)
011900150513     c                   eval      DBADEBDP   = Dato_sk(20)
012000150513      **
012100150513      * ? Nel 16° campo :                            (Oblligatorio)  Importo
012200150513     C                   add       16            sk
012300150513     c                   If        DatoNum_sk(sk) = 'N' or
012400150513     c                             LunDato_sk(sk) =  0
012500150513     c                   Else
012600150513     c                   eval      DBAQTY   = atoi(%TRIM(Dato_sk(sk)))
012700150513     c                   End
012800150513      **
012900150513      **
013000150513      * ? Nel 17° campo : Importo                    (Oblligatorio)  Importo
013100150513      **
013200150513      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
013300150513      **     IL DEBITO È PRECEDUTO DAL (-)
013400150513     C                   z-add     17            sk
013500150513      **
013600150513     c                   If        LunDato_sk(sk) >  0
013700150513     c                   movel     'C'           deb_cred
013800150513      *
013900150513     c                   if        %subst(Dato_sk(sk):1:1) = '-'
014000150513     c                   movel     'D'           deb_cred
014100150513     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
014200150513     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
014300150513     c                   end
014400150513      **
014500150513      **  TOGLIE il PUNTO e lo sostituisce
014600150513     c     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
014700150513      **
014800150513      **  se ci sono altri caratteri al posto dei numeri e della virgola
014900150513     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
015000150513     c                   if        %Found
015100150513     c                              and N <= LunDato_sk(sk)
015200150513      * ?-----> Errore
015300150513     c                   else
015400150513     c                   eval(H)   DBAVAL   = atof(%TRIM(Dato_sk(sk)))
015500150513     c                   end
015600150513     c                   end
015700150513      **
015800150513      * ? Nel 18° campo :                            (Oblligatorio)  Importo
015900150513     C                   add       18            sk
016000150513     c                   If        DatoNum_sk(sk) = 'N' or
016100150513     c                             LunDato_sk(sk) =  0
016200150513     c                   Else
016300150513     c                   eval      DBAVAT   = atoi(%TRIM(Dato_sk(sk)))
016400150513     c                   End
016500150513      **
016600150513      **
016700150513      **
016800150513      * ? Nel 21° campo : Importo                    (Oblligatorio)  Importo
016900150513      **
017000150513      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
017100150513      **     IL DEBITO È PRECEDUTO DAL (-)
017200150513     C                   z-add     21            sk
017300150513      **
017400150513     c                   If        LunDato_sk(sk) >  0
017500150513     c                   movel     'C'           deb_cred
017600150513      *
017700150513     c                   if        %subst(Dato_sk(sk):1:1) = '-'
017800150513     c                   movel     'D'           deb_cred
017900150513     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
018000150513     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
018100150513     c                   end
018200150513      **
018300150513      **  TOGLIE il PUNTO e lo sostituisce
018400150513     c     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
018500150513      **
018600150513      **  se ci sono altri caratteri al posto dei numeri e della virgola
018700150513     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
018800150513     c                   if        %Found
018900150513     c                              and N <= LunDato_sk(sk)
019000150513      * ?-----> Errore
019100150513     c                   else
019200150513     c                   eval(H)   DBAAMO   = atof(%TRIM(Dato_sk(sk)))
019300150513     c                   end
019400150513     c                   end
019500150513      **
019600150513     c                   write     WFDPDBA0
019700140319     c
019800081202     c                   Endsr
019900081202      * ?____________________________________________________________       */
020000081202      *?      Suddivide i campi della riga con carattere divisorio (;)
020100081202      * ?____________________________________________________________       */
020200081202     c     Split_Rec     Begsr
020300081202      **
020400081202     c                   clear                   Nr_campi
020500081202     c                   clear                   Campi_Record
020600081202     c                   clear                   Campi_Lunghi
020700081202     c                   clear                   Campi_Numerici
020800081202     c                   clear                   Campi_Decimali
020900081202     c                   movel     ';'           Separa_campi
021000150513     c                   movel     '.'           Decimal_Separ
021100081202      *
021200081202     c                   call      'FIEU00R'
021300081202      * ?     Input
021400081202     c                   parm                    Separa_campi
021500081202     c                   parm                    Decimal_Separ
021600081202     c                   parm                    dati
021700081202      * ?     Output  su schiere di 100 elementi
021800081202     c                   parm                    Nr_campi
021900081202     c                   parm                    Campi_Record                   * i dati
022000081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
022100081202     c                   parm                    Campi_Numerici                 * se numerici
022200081202     c                   parm                    Campi_Decimali                 * quanti decimali
022300081202      **
022400081202     c                   Endsr
