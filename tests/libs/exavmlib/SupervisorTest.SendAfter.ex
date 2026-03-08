#
# This file is part of AtomVM.
#
# Copyright 2026 The AtomVM Contributors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule SupervisorTest.SendAfter do
  use GenServer

  def start_link({parent, ref}) do
    GenServer.start_link(__MODULE__, {parent, ref})
  end

  def init({parent, ref}) do
    :erlang.send_after(1, self(), :show_hello)
    :erlang.send_after(5000, self(), :show_cat)
    send(parent, {:send_after_child_started, ref, self()})

    {:ok, %{parent: parent}}
  end

  def handle_info(msg, %{parent: parent} = state) do
    send(parent, {:send_after_message, self(), msg})
    {:noreply, state}
  end
end
