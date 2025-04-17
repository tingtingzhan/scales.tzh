

# Functions \link[scales]{percent} and \link[scales]{percent_format} are superseded by \link[scales]{label_percent}.
identical(scales::percent_format, scales::label_percent) |> stopifnot()

# \link[scales]{number_format} is superseded by \link[scales]{label_number}.
# even though \link[scales]{label_percent} has [number_format()] inside!!!
identical(scales::number_format, scales::label_number) |> stopifnot()




identical(scales::trans_new, scales::new_transform) |> stopifnot()
# but [new_transform()] is used in function definitions in \pkg{scales}

identical(scales::as.trans, scales::as.transform) |> stopifnot()

identical(scales::log_trans, scales::transform_log) |> stopifnot()
# ?scales::as.transform (e.g., inside ?ggplot2::coord_trans)
# does `paste0('transform_', x)`  
identical(scales::exp_trans, scales::transform_exp) |> stopifnot()

identical(scales::transform_probability, scales::probability_trans) |> stopifnot()
identical(scales::transform_logit, scales::logit_trans) |> stopifnot()



identical(scales::hue_pal, scales::pal_hue) |> stopifnot()
# all examples inside are using [pal_hue].
# [hue_pal] might be only for back-compatibility

