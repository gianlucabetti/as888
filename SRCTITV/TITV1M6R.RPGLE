000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090123     Ftivgdtmp  uF   E             DISK
000500090123     Ftivawwwt  uf a E             DISK
000600991027
000700090203     D tnvasds       e ds                  extname(tnvas00t)
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090203
001200090203     D ds_header       ds
001300090203     D  ds_h_start                    5a   inz('START')
001400090203     D  ds_h_version                  1s 0 inz(2)
001500090203     D  ds_h_partner                 25a   inz('BARTOLINI')
001600090203     D  ds_h_counter                  7s 0 inz
001700090203     D  ds_h_date                     8s 0 inz
001800090203     D  ds_h_biz                     20a   inz
001900090203
002000090203     D ds_footer       ds
002100090203     D  ds_f_end                      3a   inz('END')
002200090203     D  ds_f_crc                      7s 0 inz
002300090203     D  ds_f_amount                  12s 5 inz
002400090203     D  ds_f_currency                 3a   inz('EUR')
002500090203
002600090203     D ds_body         ds
002700090203     D  ds_b_line                     4a   inz('LINE')
002800090203     D  ds_b_refpay1                 50a   inz
002900090203     D  ds_b_refpay2                 50a   inz
003000090203     D  ds_b_amount                  12s 5 inz
003100090203     D  ds_b_currency                 3a   inz('EUR')
003200090203     D  ds_b_exchrate                12s 5 inz(1)
003300090203     D  ds_b_date                     8s 0 inz
003400090203     D  ds_b_orderid                 50a   inz
003500090203     D  ds_b_i_comm                  12s 5 inz
003600090203     D  ds_b_p_comm                  12s 5 inz
003700090203     D  ds_b_sec_dep                 12s 5 inz
003800090203     D  ds_b_descript               100a   inz
003900090203     D  ds_b_freetext               100a   inz
004000090203     D  ds_b_type                     1a   inz('P')
004100090203
004200090203     D ds_Key_bolla    ds
004300090203     D  wvasAAS                       4
004400090203     D  wvasLNP                       3
004500090203     D  wvasNRS                       2
004600090203     D  wvasNSP                       7
004700090123
004800090203
004900991027
005000030908     C                   EXSR      chkRECFILMBR
005100991027     C                   EXSR      traduci
005200921023     C                   SETON                                        LR
005300030908
005400090203
005500090123
005600030908     C     chkRECFILMBR  BEGSR
005700030908     C*
005800090123     C                   READ      TIVAWWWT                               55
005900030908     C*
006000030908     C                   ENDSR
006100991027
006200090203
006300090123
006400991027     C     traduci       BEGSR
006500051007     C*
006600051007     C* Verifico se richiesto un particolare carattere d separatore campi
006700090203     C                   IF        %subst(prmppt:1:1) <> *blanks
006800090203     C                   EVAL      wSepCampi = %subst(prmppt:1:1)
006900051007     C                   ELSE
007000090203     C                   MOVEL     ';'           wSepCampi         1
007100051007     C                   ENDIF
007200030325     C*
007300090203     C* Se file/membro output già pieno battezzo ad errore
007400090203     C                   IF        *IN55 = *OFF
007500090203     C                   EVAL      wrkesito = '1'
007600030325     C                   ENDIF
007700991027     C*
007800090203     C* Se tutto ok => procedo
007900090203     C                   IF        wrkesito = *blanks
008000090123     C                   READ      tivgdtmp
008100090203     C                   movel     vgddta        tnvasds
008200090203     C*
008300090203     C* Scarico il buffer del record d intestazione
008400090203     C                   exsr      wriHeader
008500090203     C*
008600090123     C                   DOW       not %eof(tivgdtmp)
008700090203     C*
008800090203     C* Incremento il contatore dei record d dettaglio restituiti
008900090203     C                   add       1             ds_f_crc
009000090203     C*
009100090203     C* Incremento la sommatoria del torale importo devoluto
009200090203     C                   eval      ds_f_amount = ds_f_amount + vasCAS
009300090203     C*
009400090203     C* Valorizzo il record d dettaglio corrente
009500090203     C                   exsr      wriBody
009600930409     C*
009700090123     C                   DELETE    tivgd000
009800090123     C                   READ      tivgdtmp
009900090203     C                   movel     vgddta        tnvasds
010000090203     C*
010100030325     C                   ENDDO
010200090203     C*
010300090203     C* Scarico il buffer del record d chiusura
010400090203     C                   exsr      wriFooter
010500090203     C*
010600090203     C                   EVAL      wrkesito = '0'
010700090203     C*
010800090203     C                   ENDIF
010900991027     C*
011000910830     C                   ENDSR
011100090123
011200090203
011300090123
011400090203     C     wriHeader     BEGSR
011500090203     C*
011600090203     C                   clear                   tivaw000
011700090203     C                   eval      ds_h_counter = vasNDP
011800090203     C                   eval      ds_h_date    = vasDDP
011900090203     C                   if        vasKSC = 0054770
012000090203     C                   eval      ds_h_biz = 'Pix'
012100090203     C                   endif
012200090203     C                   if        vasKSC = 0054772
012300090203     C                   eval      ds_h_biz = 'PixPro'
012400090203     C                   endif
012500090203     C*
012600090203     C                   eval      vawdta=
012700090205     C                                  %trim(ds_h_start)
012800090205     C                                + wSepCampi
012900090205     C                                + %trim(%char(ds_h_version))
013000090205     C                                + wSepCampi
013100090205     C                                + %trim(ds_h_partner)
013200090205     C                                + wSepCampi
013300090205     C                                + %trim(%char(ds_h_counter))
013400090205     C                                + wSepCampi
013500090205     C                                + %trim(%char(ds_h_date))
013600090205     C                                + wSepCampi
013700090205     C                                + %trim(ds_h_biz)
013800090203     C                   write     tivaw000
013900090203     C*
014000090203     C                   ENDSR
014100090203
014200090203
014300090203
014400090203     C     wriFooter     BEGSR
014500090203     C*
014600090203     C                   clear                   tivaw000
014700090203     C*
014800090203     C                   eval      vawdta=
014900090205     C                                  %trim(ds_f_end)
015000090205     C                                + wSepCampi
015100090205     C                                + '0'+%trim(%char(ds_f_crc))
015200090205     C                                + wSepCampi
015300090205     C                                + '0'+%trim(%char(ds_f_amount))
015400090205     C                                + wSepCampi
015500090205     C                                + %trim(ds_f_currency)
015600090203     C                   write     tivaw000
015700090203     C*
015800090203     C                   ENDSR
015900090203
016000090203
016100090203
016200090203     C     wriBody       BEGSR
016300090203     C*
016400090203     C* Inizializzo la ds d dettaglio
016500090203     C                   reset                   ds_body
016600090203     C*
016700090203     C                   movel     vasAAS        wvasAAS
016800090203     C                   movel     vasLNP        wvasLNP
016900090203     C                   movel     vasNRS        wvasNRS
017000090203     C                   movel     vasNSP        wvasNSP
017100090203     C*
017200090203     C                   movel     vasRMN        ds_b_refpay1
017300090203     C                   movel     ds_Key_bolla  ds_b_refpay2
017400090203     C                   z-add     vasCAS        ds_b_amount
017500090203     C                   z-add     vasDDP        ds_b_date
017600090203     C                   movel     vasRMA        ds_b_orderid
017700090203     C*
017800090203     C                   clear                   tivaw000
017900090203     C*
018000090203     C                   eval      vawdta=
018100090205     C                                  %trim(ds_b_line)
018200090205     C                                + wSepCampi
018300090205     C                                + %trim(ds_b_refpay1)
018400090205     C                                + wSepCampi
018500090205     C                                + %trim(ds_b_refpay2)
018600090205     C                                + wSepCampi
018700090205     C                                + '0'+%trim(%char(ds_b_amount))
018800090205     C                                + wSepCampi
018900090205     C                                + %trim(ds_b_currency)
019000090205     C                                + wSepCampi
019100090205     C                                + %trim(%char(ds_b_exchrate))
019200090205     C                                + wSepCampi
019300090205     C                                + %trim(%char(ds_b_date))
019400090205     C                                + wSepCampi
019500090205     C                                + %trim(ds_b_orderid)
019600090205     C                                + wSepCampi
019700090205     C                                + %trim(%char(ds_b_i_comm))
019800090205     C                                + wSepCampi
019900090205     C                                + %trim(%char(ds_b_p_comm))
020000090205     C                                + wSepCampi
020100090205     C                                + %trim(%char(ds_b_sec_dep))
020200090205     C                                + wSepCampi
020300090205     C                                + %trim(ds_b_descript)
020400090205     C                                + wSepCampi
020500090205     C                                + %trim(ds_b_freetext)
020600090205     C                                + wSepCampi
020700090205     C                                + %trim(ds_b_type)
020800090205     C                   write     tivaw000
020900090203     C*
021000090203     C                   ENDSR
021100090203
021200090203
021300090203
021400090123
021500991027      /TITLE Operazioni iniziali.
021600991027     C     *inzsr        BEGSR
021700991027     C*
021800991027     C     *ENTRY        PLIST
021900991027     C                   parm                    prmppt
022000991027     C     wrkesito      parm      wrkesito      prmesito
022100991027     C*
022200991027     C                   ENDSR
