[![CI](https://github.com/slepher/rebar3_erlando/actions/workflows/ci.yml/badge.svg?branch=master&event=push)](https://github.com/slepher/rebar3_erlando/actions/workflows/ci.yml?query=branch%3Amaster)

[![CI](https://github.com/slepher/rebar3_erlando/actions/workflows/release.yml/badge.svg?branch=0.4.3&event=push)](https://github.com/slepher/rebar3_erlando/actions/workflows/release.yml?query=branch%3A0.4.3)

erlando
=====

见 https://github.com/slepher/erlando

rebar3_erlando
-----

为 erlando 增加命令：

    $ rebar3 erlando compile

typeclass.beam 现在由 rebar3_erlando rebar3 插件在编译期生成。

从 0.4.0 起，插件在 init 时自动注入 post-compile hook
`{post, [{compile, {erlando, compile}}]}` 到项目状态，项目无需自行声明
该 hook；hook 每次构建执行一次（项目级），把生成的 typeclass.beam 写入
erlando app 的 out_dir。Per-app 的 hook 执行是 no-op。

typeclass registry 每次编译都从所有依赖的 beam 重建，而不是在构建之间
累积状态，因此重复编译时 registry 保持正确。

erlando_typeclass:register_application/1 已不再使用。

从 0.3.0 起，插件优先使用带版本的 `erlando_instance_meta` 属性。这些
属性定义精确的 `{Type, Typeclass}` 映射，并校验必需的回调、冲突、
生成的 capability 适配器与分发覆盖。没有新元数据的模块继续使用旧的
`erlando_type x behaviour` 注册规则。
