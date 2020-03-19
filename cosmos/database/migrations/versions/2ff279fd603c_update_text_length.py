"""Update text length

Revision ID: 2ff279fd603c
Revises: 7c8710da35ef
Create Date: 2020-03-19 17:12:10.109633

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '2ff279fd603c'
down_revision = '7c8710da35ef'
branch_labels = None
depends_on = None


def upgrade():
    op.alter_column('object_contexts', 'content', type_=sa.Text(2**32-1), existing_nullable=True, existing_comment='', existing_autoincrement=False, server_default=None)

